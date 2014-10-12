unit BeRoSoundSystem;
(*************************************
** 2-clause simplified BSD license ***
**************************************
**
** Copyright 2007-2011 Benjamin Rosseaux. All rights reserved.
**
** Redistribution and use in source and binary forms, with or without modification, are
** permitted provided that the following conditions are met:
**
**    1. Redistributions of source code must retain the above copyright notice, this list of
**       conditions and the following disclaimer.
**
**    2. Redistributions in binary form must reproduce the above copyright notice, this list
**       of conditions and the following disclaimer in the documentation and/or other materials
**       provided with the distribution.
**
** THIS SOFTWARE IS PROVIDED BY BENJAMIN ROSSEAUX ``AS IS'' AND ANY EXPRESS OR IMPLIED
** WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
** FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL BENJAMIN ROSSEAUX OR
** CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
** CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
** SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
** ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
** NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
** ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
**
** The views and conclusions contained in the software and documentation are those of the
** authors and should not be interpreted as representing official policies, either expressed
** or implied, of Benjamin Rosseaux.
*)
{$ifdef fpc}
 {$mode delphi}
 {$warnings off}
 {$hints off}
 {$ifdef cpui386}
  {$define cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef fpc_little_endian}
  {$define little_endian}
 {$else}
  {$ifdef fpc_big_endian}
   {$define big_endian}
  {$endif}
 {$endif}
 {$ifdef fpc_has_internal_sar}
  {$define HasSAR}
 {$endif}
{$else}
 {$define little_endian}
 {$ifndef cpu64}
  {$define cpu64}
 {$endif}
 {$optimization on}
 {$undef HasSAR}
 {$define UseDIV}
{$endif}
{$overflowchecks off}
{$rangechecks off}

interface

uses BeRoXM,BeRoCriticalSection;

const BSSSampleFixUp=1024;

      BSSSoundLoopModeNONE=0;
      BSSSoundLoopModeFORWARD=1;
      BSSSoundLoopModePINGPONG=2;
      BSSSoundLoopModeBACKWARD=3;

      BSSXMTrackInterpolationNEAREST=0;
      BSSXMTrackInterpolationLINEAR=1;
      BSSXMTrackInterpolationCUBICSPLINE=2;
      BSSXMTrackInterpolationWINDOWEDFIR=3;

type PBSSSampleValue=^TBSSSampleValue;
     TBSSSampleValue=longint;

     PBSSSampleValues=^TBSSSampleValues;
     TBSSSampleValues=array[0..($7ffffff0 div sizeof(TBSSSampleValue))-1] of TBSSSampleValue;

     PBSSSampleLoop=^TBSSSampleLoop;
     TBSSSampleLoop=record
      Mode:integer;
      StartSample:integer;
      EndSample:integer;
     end;

     TBSSSample=class;
     TBeRoSoundSystem=class;

     TBSSSampleVoice=class
      public
       Sample:TBSSSample;
       Active:boolean;
       KeyOff:boolean;
       Backwards:boolean;
       Volume:integer;
       VolLeftCurrent:integer;
       VolRightCurrent:integer;
       Panning:integer;
       VolumeRampingRemain:integer;
       Age:int64;
       Position:int64;
       Increment:int64;
       LastLeft:integer;
       LastRight:integer;
       NewLastLeft:integer;
       NewLastRight:integer;
       constructor Create(ASample:TBSSSample);
       destructor Destroy; override;
       procedure Init(AVolume,APanning,ARate:integer);
       procedure MixTo(Buffer:PBSSSampleValues;MixVolume:integer);
     end;

     TBSSSampleVoices=array of TBSSSampleVoice;

     TBSSSample=class
      public
       SoundSystem:TBeRoSoundSystem;
       Data:PBSSSampleValues;
       SampleLength:integer;
       SampleRate:integer;
       Loop:TBSSSampleLoop;
       SustainLoop:TBSSSampleLoop;
       Voices:TBSSSampleVoices;
       constructor Create(ASoundSystem:TBeRoSoundSystem);
       destructor Destroy; override;
       procedure FixUp;
       procedure SetPolyphony(Polyphony:integer);
       function Play(Volume,Panning,Rate:integer):integer;
       procedure Stop(VoiceNumber:integer);
       procedure KeyOff(VoiceNumber:integer);
       procedure SetVolume(VoiceNumber,Volume:integer);
       procedure SetPanning(VoiceNumber,Panning:integer);
       procedure SetRate(VoiceNumber,Rate:integer);
       procedure MixTo(Buffer:PBSSSampleValues;MixVolume:integer);
     end;

     PBSSSamples=^TBSSSamples;
     TBSSSamples=array[byte] of TBSSSample;

     PBSSXMTracks=^TBSSXMTracks;
     TBSSXMTracks=array[byte] of TBeRoXM;

     TBeRoSoundSystem=class
      public
       Samples:TBSSSamples;
       XMTracks:TBSSXMTracks;
       SampleRate:integer;
       Channels:integer;
       Bits:integer;
       BufferSamples:integer;
       BufferChannelSamples:integer;
       BufferOutputChannelSamples:integer;
       MixingBufferSize:integer;
       OutputBufferSize:integer;
       MixingBuffer:PBSSSampleValues;
       OutputBuffer:pointer;
       MasterVolume:integer;
       MusicVolume:integer;
       SampleVolume:integer;
       RampingSamples:integer;
       AGCActive:boolean;
       AGC:integer;
       AGCCounter:integer;
       AGCInterval:integer;
       CriticalSection:TBeRoCriticalSection;
       constructor Create(ASampleRate,AChannels,ABits,ABufferSamples:integer);
       destructor Destroy; override;
       procedure SetMixerMasterVolume(NewVolume:single);
       procedure SetMixerMusicVolume(NewVolume:single);
       procedure SetMixerSampleVolume(NewVolume:single);
       procedure SetMixerAGC(Enabled:boolean);
       procedure FillBuffer;
       function LoadSample(SampleSlotNumber:byte;DataPtr:pointer;DataLen,Polyphony:integer):boolean;
       function UnloadSample(SampleSlotNumber:byte):boolean;
       function PlaySample(SampleSlotNumber:byte;Volume,Panning,Rate:single):integer;
       function IsSamplePlaying(SampleSlotNumber:byte):boolean;
       function IsSampleVoicePlaying(SampleSlotNumber:byte;VoiceNumber:integer):boolean;
       procedure StopSample(SampleSlotNumber:byte;VoiceNumber:integer);
       procedure KeyOffSample(SampleSlotNumber:byte;VoiceNumber:integer);
       procedure SetSampleVolume(SampleSlotNumber:byte;VoiceNumber:integer;Volume:single);
       procedure SetSamplePanning(SampleSlotNumber:byte;VoiceNumber:integer;Panning:single);
       procedure SetSampleRate(SampleSlotNumber:byte;VoiceNumber:integer;Rate:single);
       function LoadXMTrack(XMTrackSlotNumber:byte;DataPtr:pointer;DataLen:integer;Looping:boolean;Interpolation:integer=BSSXMTrackInterpolationLINEAR):boolean;
       function UnloadXMTrack(XMTrackSlotNumber:byte):boolean;
       function PlayXMTrack(XMTrackSlotNumber:byte):boolean;
       function StopXMTrack(XMTrackSlotNumber:byte):boolean;
       function IsXMTrackPlaying(XMTrackSlotNumber:byte):boolean;
     end;

implementation

const PositionShift=32;
      PositionFactor:INT64=$100000000;//(1 shl PositionShift);
      PositionMask=$FFFFFFFF;//INT64(1 shl PositionShift)-1;
      PositionDivFactor=1/$100000000;//(1 shl PositionShift);

      PositionAllRemainBits=14;
      PositionAllRemainFactor=1 shl PositionAllRemainBits;

{$ifndef fpc}
type ptruint=longword;
     ptrint=longint;
{$endif}

function SwapWordLittleEndian(Value:word):word; {$ifdef cpu386}register;{$endif}
begin
{$ifdef big_endian}
 result:=((Value and $ff00) shr 8) or ((Value and $ff) shl 8);
{$else}
 result:=Value;
{$endif}
end;

function SwapDWordLittleEndian(Value:longword):longword; {$ifdef cpu386}register;{$endif}
begin
{$ifdef big_endian}
 result:=((Value and $ff000000) shr 24) or ((Value and $00ff0000) shr 8) or
         ((Value and $0000ff00) shl 8) or ((Value and $000000ff) shl 24);
{$else}
 result:=Value;
{$endif}
end;

function SwapWordBigEndian(Value:word):word; {$ifdef cpu386}register;{$endif}
begin
{$ifdef little_endian}
 result:=((Value and $ff00) shr 8) or ((Value and $ff) shl 8);
{$else}
 result:=Value;
{$endif}
end;

function SwapDWordBigEndian(Value:longword):longword; {$ifdef cpu386}register;{$endif}
begin
{$ifdef little_endian}
 result:=((Value and $ff000000) shr 24) or ((Value and $00ff0000) shr 8) or
         ((Value and $0000ff00) shl 8) or ((Value and $000000ff) shl 24);
{$else}
 result:=Value;
{$endif}
end;

procedure SwapLittleEndianData16(var Data); {$ifdef cpu386}register;{$endif}
{$ifdef big_endian}
var Value:word absolute Data;
begin
 Value:=((Value and $ff00) shr 8) or ((Value and $ff) shl 8);
{$else}
begin
{$endif}
end;

procedure SwapLittleEndianData32(var Data); {$ifdef cpu386}register;{$endif}
{$ifdef big_endian}
var Value:longword absolute Data;
begin
 Value:=((Value and $ff000000) shr 24) or ((Value and $00ff0000) shr 8) or
        ((Value and $0000ff00) shl 8) or ((Value and $000000ff) shl 24);
{$else}
begin
{$endif}
end;

procedure SwapBigEndianData16(var Data); {$ifdef cpu386}register;{$endif}
{$ifdef little_endian}
var Value:word absolute Data;
begin
 Value:=((Value and $ff00) shr 8) or ((Value and $ff) shl 8);
{$else}
begin
 result:=Value;
{$endif}
end;

procedure SwapBigEndianData32(var Data); {$ifdef cpu386}register;{$endif}
{$ifdef little_endian}
var Value:longword absolute Data;
begin
 Value:=((Value and $ff000000) shr 24) or ((Value and $00ff0000) shr 8) or
        ((Value and $0000ff00) shl 8) or ((Value and $000000ff) shl 24);
{$else}
begin
{$endif}
end;

function SAR(Value,Shift:integer):integer;
{$ifdef cpu386}
{$ifdef fpc} assembler; inline;
asm
 mov ecx,edx
 SAR eax,cl
end ['eax','edx','ecx'];
{$else} assembler; register;
asm
 mov ecx,edx
 SAR eax,cl
end;
{$endif}
{$else}
{$ifdef cpuarm} assembler; inline;
asm
 mov r0,r0,asr R1
end ['r0','R1'];
{$else}{$ifdef fpc}inline;{$endif}
begin
{$ifdef HasSAR}
 result:=SARLongint(Value,Shift);
{$else}
 Shift:=Shift and 31;
 result:=(longword(Value) shr Shift) or (longword(longint(longword(0-longword(longword(Value) shr 31)) and longword(0-longword(ord(Shift<>0))))) shl (32-Shift));
{$endif}
end;
{$endif}
{$endif}

constructor TBSSSampleVoice.Create(ASample:TBSSSample);
begin
 inherited Create;
 Sample:=ASample;
 Active:=false;
 LastLeft:=0;
 LastRight:=0;
 NewLastLeft:=0;
 NewLastRight:=0;
end;

destructor TBSSSampleVoice.Destroy;
begin
 inherited Destroy;
end;

procedure TBSSSampleVoice.Init(AVolume,APanning,ARate:integer);
var i:int64;
begin
 Active:=true;
 KeyOff:=false;
 Backwards:=false;
 Volume:=AVolume;
 Panning:=APanning;
 i:=Sample.SampleRate;
 i:=i*ARate;
 i:=i*65536;
 i:=i div Sample.SoundSystem.SampleRate;
 Increment:=i;
 Age:=0;
 Position:=0;
 VolumeRampingRemain:=0;
 VolLeftCurrent:=0;
 VolRightCurrent:=0;
 inc(LastLeft,NewLastLeft);
 inc(LastRight,NewLastRight);
 NewLastLeft:=0;
 NewLastRight:=0;
end;

procedure TBSSSampleVoice.MixTo(Buffer:PBSSSampleValues;MixVolume:integer);
const i32=int64($100000000);
type plongint=^longint;
 function GetSampleLength(CountSamplesValue:integer):integer;
 var SmpLen,SmpLoopStart,SmpLoopEnd,CountSamples,MaxSamples,Difference:int64;
     LoopMode:integer;
 begin
  SmpLen:=Sample.SampleLength*i32;
  if (Sample.SustainLoop.Mode<>BSSSoundLoopModeNONE) and not KeyOff then begin
   LoopMode:=Sample.SustainLoop.Mode;
   SmpLoopStart:=Sample.SustainLoop.StartSample*i32;
   SmpLoopEnd:=Sample.SustainLoop.EndSample*i32;
  end else if Sample.Loop.Mode<>BSSSoundLoopModeNONE then begin
   LoopMode:=Sample.Loop.Mode;
   SmpLoopStart:=Sample.Loop.StartSample*i32;
   SmpLoopEnd:=Sample.Loop.EndSample*i32;
  end else begin
   LoopMode:=BSSSoundLoopModeNONE;
   SmpLoopStart:=0;
   SmpLoopEnd:=SmpLen;
  end;
  if LoopMode<>BSSSoundLoopModeNONE then begin
   if Position<SmpLoopStart then begin
    if Backwards then begin
     if LoopMode=BSSSoundLoopModePINGPONG then begin
      Position:=SmpLoopStart-(SmpLoopStart-Position);
      Backwards:=false;
      if (Position<SmpLoopStart) or (Position>=((SmpLoopStart+SmpLoopEnd) div 2)) then begin
       Position:=SmpLoopStart;
      end;
     end else if LoopMode=BSSSoundLoopModeBACKWARD then begin
      Position:=SmpLen-(SmpLoopStart-Position);
      if Position>=SmpLoopEnd then begin
       Position:=Position+(SmpLoopStart-SmpLoopEnd);
       if Position<SmpLoopStart then begin
        Position:=SmpLoopStart;
       end;
      end;
     end else begin
      Position:=SmpLoopEnd-(SmpLoopStart-Position);
      Backwards:=false;
      if Position>=SmpLoopEnd then begin
       Position:=Position+(SmpLoopStart-SmpLoopEnd);
       if Position<SmpLoopStart then begin
        Position:=SmpLoopStart;
       end;
      end;
     end;
    end else begin
     if Position<0 then begin
      Position:=0;
     end;
    end;
   end else if Position>=SmpLoopEnd then begin
    if LoopMode=BSSSoundLoopModeBACKWARD then begin
     Backwards:=true;
    end else if LoopMode=BSSSoundLoopModePINGPONG then begin
     Position:=SmpLoopEnd-(Position-SmpLoopEnd);
     Backwards:=true;
     if (Position<SmpLoopStart) or (Position>=((SmpLoopStart+SmpLoopEnd) div 2)) then begin
      Position:=SmpLoopStart;
     end;
    end else begin
     Position:=Position+(SmpLoopStart-SmpLoopEnd);
     if Position<SmpLoopStart then begin
      Position:=SmpLoopStart;
     end;
    end;
   end;
  end;
  if (Position<0) OR (Position>=SmpLen) OR
     ((Position<SmpLoopStart) AND
      ((Position<0) OR Backwards)) then begin
   result:=0;
   exit;
  end;
  CountSamples:=CountSamplesValue;
  MaxSamples:=PositionAllRemainFactor div ((Increment div PositionFactor)+1);
  if MaxSamples<2 then begin
   MaxSamples:=2;
  end;
  if CountSamplesValue>MaxSamples then begin
   CountSamplesValue:=MaxSamples;
  end;
  if Backwards then begin
   Difference:=(Position-(Increment*(CountSamplesValue-1)));
   if Difference<SmpLoopStart then begin
    CountSamples:=((Position-SmpLoopStart-1) div Increment)+1;
   end;
  end else begin
   Difference:=(Position+(Increment*(CountSamplesValue-1)));
   if Difference>=SmpLoopEnd then begin
    CountSamples:=((SmpLoopEnd-Position-1) div Increment)+1;
   end;
  end;
  if CountSamples<=1 then begin
   result:=1;
   exit;
  end else if CountSamples>CountSamplesValue then begin
   result:=CountSamplesValue;
   exit;
  end;
  result:=CountSamples;
 end;
var Pan,VolLeft,VolRight,VolLeftInc,VolRightInc,VolLeftCur,VolRightCur,Count,i,j,k,s,p,m,
    LocalLastLeft,LocalLastRight,LocalNewLastLeft,LocalNewLastRight:integer;
    Buf:plongint;
    p64,i64:int64;
    d:PBSSSampleValues;
begin
 LocalLastLeft:=LastLeft;
 LocalLastRight:=LastRight;
 if (LocalLastLeft<>0) or (LocalLastRight<>0) then begin
  Buf:=pointer(Sample.SoundSystem.MixingBuffer);
  for i:=1 to Sample.SoundSystem.BufferSamples do begin
{$ifdef HasSAR}
   dec(LocalLastLeft,SARLongint(LocalLastLeft+(SARLongint(-LocalLastLeft,31) and $ff),8));
   dec(LocalLastRight,SARLongint(LocalLastRight+(SARLongint(-LocalLastRight,31) and $ff),8));
{$else}
   dec(LocalLastLeft,sar(LocalLastLeft+(sar(-LocalLastLeft,31) and $ff),8));
   dec(LocalLastRight,sar(LocalLastRight+(sar(-LocalLastRight,31) and $ff),8));
{$endif}
   Buf^:=Buf^+{$ifdef HasSAR}SARLongint{$else}SAR{$endif}(LocalLastLeft,12);
   inc(Buf);
   Buf^:=Buf^+{$ifdef HasSAR}SARLongint{$else}SAR{$endif}(LocalLastRight,12);
   inc(Buf);
  end;
 end;
 if not Active then begin
  LastLeft:=LocalLastLeft;
  LastRight:=LocalLastRight;
  exit;
 end;
 Pan:=Panning+65536;
 if Pan<0 then begin
  Pan:=0;
 end else if Pan>=131072 then begin
  Pan:=131071;
 end;
 MixVolume:=SAR(Volume*MixVolume,16);
 VolLeft:=SAR((131072-Pan)*MixVolume,17);
 VolRight:=SAR(Pan*MixVolume,17);
 if VolLeft<0 then begin
  VolLeft:=0;
 end else if VolLeft>=4096 then begin
  VolLeft:=4095;
 end;
 if VolRight<0 then begin
  VolRight:=0;
 end else if VolRight>=4096 then begin
  VolRight:=4095;
 end;
 VolLeft:=VolLeft shl 12;
 VolRight:=VolRight shl 12;
 if VolumeRampingRemain=0 then begin
  VolLeftCur:=VolLeft;
  VolRightCur:=VolRight;
  VolLeftInc:=0;
  VolRightInc:=0;
 end else begin
  VolLeftCur:=VolLeftCurrent;
  VolRightCur:=VolRightCurrent;
  VolLeftInc:=(VolLeft-VolLeftCur) div VolumeRampingRemain;
  VolRightInc:=(VolRight-VolRightCur) div VolumeRampingRemain;
 end;
 Count:=Sample.SoundSystem.BufferSamples;
 LocalNewLastLeft:=0;
 LocalNewLastRight:=0;
 Buf:=pointer(Sample.SoundSystem.MixingBuffer);
 d:=Sample.Data;
 i:=Count;
 while (i>0) and Active do begin
  j:=i;
  if (VolumeRampingRemain>0) and (j>=VolumeRampingRemain) then begin
   j:=VolumeRampingRemain;
  end;
  j:=GetSampleLength(j);
  if j=0 then begin
   Active:=false;
   break;
  end;
  dec(i,j);
  inc(Age,j);
  p64:=Position;
  if Backwards then begin
   i64:=-Increment;
  end else begin
   i64:=Increment;
  end;
  if VolumeRampingRemain>0 then begin
   dec(VolumeRampingRemain,j);
   for k:=1 to j do begin
    VolLeft:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(VolLeftCur,12);
    VolRight:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(VolRightCur,12);
    inc(VolLeftCur,VolLeftInc);
    inc(VolRightCur,VolRightInc);
    p:=longword(p64 shr PositionShift) shl 1;
    m:=(longword(p64 and $ffffffff) shr (PositionShift-12)) and $fff;
    s:=d^[p];
{$ifdef UseDIV}
    LocalNewLastLeft:=(longint(s+((d^[p+2]-s)*m) div 4096)*VolLeft) div 4096;
{$else}
    LocalNewLastLeft:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(longint(s+{$ifdef HasSAR}SARLongint{$else}SAR{$endif}((d^[p+2]-s)*m,12))*VolLeft,12);
{$endif}
    inc(Buf^,LocalNewLastLeft);
    inc(Buf);
    s:=d^[p+1];
{$ifdef UseDIV}
    LocalNewLastRight:=(longint(s+((d^[p+3]-s)*m) div 4096)*VolRight) div 4096;
{$else}
    LocalNewLastRight:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(longint(s+{$ifdef HasSAR}SARLongint{$else}SAR{$endif}((d^[p+3]-s)*m,12))*VolRight,12);
{$endif}
    inc(Buf^,LocalNewLastRight);
    inc(Buf);
    inc(p64,i64);
   end;
  end else begin
   VolLeft:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(VolLeftCur,12);
   VolRight:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(VolRightCur,12);
   for k:=1 to j do begin
    p:=longword(p64 shr PositionShift) shl 1;
    m:=(longword(p64 and $ffffffff) shr (PositionShift-12)) and $fff;
    s:=d^[p];
{$ifdef UseDIV}
    LocalNewLastLeft:=(longint(s+((d^[p+2]-s)*m) div 4096)*VolLeft) div 4096;
{$else}
    LocalNewLastLeft:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(longint(s+{$ifdef HasSAR}SARLongint{$else}SAR{$endif}((d^[p+2]-s)*m,12))*VolLeft,12);
{$endif}
    inc(Buf^,LocalNewLastLeft);
    inc(Buf);
    s:=d^[p+1];
{$ifdef UseDIV}
    LocalNewLastRight:=(longint(s+((d^[p+3]-s)*m) div 4096)*VolRight) div 4096;
{$else}
    LocalNewLastRight:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(longint(s+{$ifdef HasSAR}SARLongint{$else}SAR{$endif}((d^[p+3]-s)*m,12))*VolRight,12);
{$endif}
    inc(Buf^,LocalNewLastRight);
    inc(Buf);
    inc(p64,i64);
   end;
  end;
  Position:=p64;
 end;
 LocalNewLastLeft:=LocalNewLastLeft shl 12;
 LocalNewLastRight:=LocalNewLastRight shl 12;
 if (i>0) and ((LocalNewLastLeft<>0) or (LocalNewLastRight<>0)) then begin
  while i>0 do begin
{$ifdef HasSAR}
   dec(LocalNewLastLeft,SARLongint(LocalNewLastLeft+(SARLongint(-LocalNewLastLeft,31) and $ff),8));
   dec(LocalNewLastRight,SARLongint(LocalNewLastRight+(SARLongint(-LocalNewLastRight,31) and $ff),8));
{$else}
   dec(LocalNewLastLeft,sar(LocalNewLastLeft+(sar(-LocalNewLastLeft,31) and $ff),8));
   dec(LocalNewLastRight,sar(LocalNewLastRight+(sar(-LocalNewLastRight,31) and $ff),8));
{$endif}
   Buf^:=Buf^+{$ifdef HasSAR}SARLongint{$else}SAR{$endif}(LocalNewLastLeft,12);
   inc(Buf);
   Buf^:=Buf^+{$ifdef HasSAR}SARLongint{$else}SAR{$endif}(LocalNewLastRight,12);
   inc(Buf);
   dec(i);
  end;
 end;
 NewLastLeft:=LocalNewLastLeft;
 NewLastRight:=LocalNewLastRight;
 VolLeftCurrent:=VolLeftCur;
 VolRightCurrent:=VolRightCur;
 LastLeft:=LocalLastLeft;
 LastRight:=LocalLastRight;
end;

constructor TBSSSample.Create(ASoundSystem:TBeRoSoundSystem);
begin
 inherited Create;
 SoundSystem:=ASoundSystem;
 Data:=nil;
 Loop.Mode:=BSSSoundLoopModeNONE;
 SustainLoop.Mode:=BSSSoundLoopModeNONE;
 Voices:=nil;
end;

destructor TBSSSample.Destroy;
var i:integer;
begin
 for i:=0 to length(Voices)-1 do begin
  if assigned(Voices[i]) then begin
   Voices[i].Destroy;
   Voices[i]:=nil;
  end;
 end;
 SetLength(Voices,0);
 if assigned(Data) then begin
  dec(plongint(Data),2*BSSSampleFixUp);
  FreeMem(Data);
  Data:=nil;
 end;
 inherited Destroy;
end;

procedure TBSSSample.FixUp;
var Counter,LoopStart,LoopEnd:integer;
begin
 if assigned(Data) then begin
  if SampleLength>0 then begin
   for Counter:=0 to BSSSampleFixUp-1 do begin
    Data^[(-(Counter+1)*2)]:=Data^[0];
    Data^[(-(Counter+1)*2)+1]:=Data^[1];
    Data^[(SampleLength+Counter)*2]:=Data^[(SampleLength-1)*2];
    Data^[((SampleLength+Counter)*2)+1]:=Data^[((SampleLength-1)*2)+1];
   end;
   if Loop.Mode in [BSSSoundLoopModeFORWARD,BSSSoundLoopModeBACKWARD] then begin
    LoopStart:=Loop.StartSample;
    LoopEnd:=Loop.EndSample;
    if (LoopStart>0) and (LoopEnd>0) and (LoopEnd<=SampleLength) then begin
     for Counter:=0 to BSSSampleFixUp-1 do begin
      Data^[(LoopEnd+Counter)*2]:=Data^[(LoopStart+Counter)*2];
      Data^[((LoopEnd+Counter)*2)+1]:=Data^[((LoopStart+Counter)*2)+1];
     end;
    end;
   end;
   if SustainLoop.Mode in [BSSSoundLoopModeFORWARD,BSSSoundLoopModeBACKWARD] then begin
    LoopStart:=SustainLoop.StartSample;
    LoopEnd:=SustainLoop.EndSample;
    if (LoopStart>0) and (LoopEnd>0) and (LoopEnd<=SampleLength) then begin
     for Counter:=0 to BSSSampleFixUp-1 do begin
      Data^[(LoopEnd+Counter)*2]:=Data^[(LoopStart+Counter)*2];
      Data^[((LoopEnd+Counter)*2)+1]:=Data^[((LoopStart+Counter)*2)+1];
     end;
    end;
   end;
  end;
 end;
end;

procedure TBSSSample.SetPolyphony(Polyphony:integer);
var i:integer;
begin
 for i:=0 to length(Voices)-1 do begin
  if assigned(Voices[i]) then begin
   Voices[i].Destroy;
   Voices[i]:=nil;
  end;
 end;
 SetLength(Voices,Polyphony);
 for i:=0 to length(Voices)-1 do begin
  Voices[i]:=TBSSSampleVoice.Create(self);
 end;
end;

function TBSSSample.Play(Volume,Panning,Rate:integer):integer;
var BestVoice,BestVolume,i:integer;
    BestAge:int64;
begin
 BestVoice:=-1;
 for i:=0 to length(Voices)-1 do begin
  if not Voices[i].Active then begin
   BestVoice:=i;
   break;
  end;
 end;
 if BestVoice<0 then begin
  BestVolume:=$7fffffff;
  BestAge:=0;
  for i:=0 to length(Voices)-1 do begin
   if (Voices[i].Age>BestAge) or ((Voices[i].Age=BestAge) and (Voices[i].Volume<=BestVolume)) then begin
    BestAge:=Voices[i].Age;
    BestVolume:=Voices[i].Volume;
    BestVoice:=i;
   end;
  end;
 end;
 if (BestVoice>=0) and (BestVoice<length(Voices)) then begin
  Voices[BestVoice].Init(Volume,Panning,Rate);
 end;
 result:=BestVoice;
end;

procedure TBSSSample.Stop(VoiceNumber:integer);
begin
 if (VoiceNumber>=0) and (VoiceNumber<length(Voices)) then begin
  Voices[VoiceNumber].Active:=false;
 end;
end;

procedure TBSSSample.KeyOff(VoiceNumber:integer);
begin
 if (VoiceNumber>=0) and (VoiceNumber<length(Voices)) then begin
  Voices[VoiceNumber].KeyOff:=true;
  if (Loop.Mode<>BSSSoundLoopModeNONE) or (SustainLoop.Mode=BSSSoundLoopModeNONE) then begin
   Voices[VoiceNumber].Active:=true;
  end;
 end;
end;

procedure TBSSSample.SetVolume(VoiceNumber,Volume:integer);
begin
 if (VoiceNumber>=0) and (VoiceNumber<length(Voices)) then begin
  Voices[VoiceNumber].Volume:=Volume;
  Voices[VoiceNumber].VolumeRampingRemain:=SoundSystem.RampingSamples;
 end;
end;

procedure TBSSSample.SetPanning(VoiceNumber,Panning:integer);
begin
 if (VoiceNumber>=0) and (VoiceNumber<length(Voices)) then begin
  Voices[VoiceNumber].Panning:=Panning;
  Voices[VoiceNumber].VolumeRampingRemain:=SoundSystem.RampingSamples;
 end;
end;

procedure TBSSSample.SetRate(VoiceNumber,Rate:integer);
var i:int64;
begin
 if (VoiceNumber>=0) and (VoiceNumber<length(Voices)) then begin
  i:=SampleRate;
  i:=i*Rate;
  i:=i*65536;
  i:=i div SoundSystem.SampleRate;
  Voices[VoiceNumber].Increment:=i;
 end;
end;

procedure TBSSSample.MixTo(Buffer:PBSSSampleValues;MixVolume:integer);
var i:integer;
begin
 for i:=0 to length(Voices)-1 do begin
  if assigned(Voices[i]) then begin
   Voices[i].MixTo(Buffer,MixVolume);
  end;
 end;
end;

constructor TBeRoSoundSystem.Create(ASampleRate,AChannels,ABits,ABufferSamples:integer);
begin
 inherited Create;
 CriticalSection:=TBeRoCriticalSection.Create;
 SampleRate:=ASampleRate;
 Channels:=AChannels;
 Bits:=ABits;
 BufferSamples:=ABufferSamples;
 BufferChannelSamples:=BufferSamples*2;
 BufferOutputChannelSamples:=BufferSamples*Channels;
 MixingBufferSize:=(BufferSamples*2*32) shr 3;
 OutputBufferSize:=(BufferSamples*Channels*Bits) shr 3;
 GetMem(MixingBuffer,MixingBufferSize);
 GetMem(OutputBuffer,OutputBufferSize);
 FillChar(Samples,sizeof(TBSSSamples),#0);
 FillChar(XMTracks,sizeof(TBSSXMTracks),#0);
 MasterVolume:=4096;
 MusicVolume:=4096;
 SampleVolume:=4096;
 AGCActive:=true;
 AGC:=256;
 AGCCounter:=0;
 AGCInterval:=(SampleRate*25) div 1000;
 if AGCInterval<1 then begin
  AGCInterval:=1;
 end;
 RampingSamples:=(120*44100) div SampleRate;
 if RampingSamples<1 then begin
  RampingSamples:=1;
 end;
end;

destructor TBeRoSoundSystem.Destroy;
var i:integer;
begin
 for i:=low(TBSSSamples) to high(TBSSSamples) do begin
  if assigned(Samples[i]) then begin
   Samples[i].Destroy;
   Samples[i]:=nil;
  end;
 end;
 for i:=low(TBSSXMTracks) to high(TBSSXMTracks) do begin
  if assigned(XMTracks[i]) then begin
   XMTracks[i].Destroy;
   XMTracks[i]:=nil;
  end;
 end;
 FreeMem(MixingBuffer);
 FreeMem(OutputBuffer);
 CriticalSection.Destroy;
 inherited Destroy;
end;

procedure TBeRoSoundSystem.SetMixerMasterVolume(NewVolume:single);
begin
 CriticalSection.Enter;
 try
  MasterVolume:=round(NewVolume*4096);
 finally
  CriticalSection.Leave;
 end;
end;

procedure TBeRoSoundSystem.SetMixerMusicVolume(NewVolume:single);
begin
 CriticalSection.Enter;
 try
  MusicVolume:=round(NewVolume*4096);
 finally
  CriticalSection.Leave;
 end;
end;

procedure TBeRoSoundSystem.SetMixerSampleVolume(NewVolume:single);
begin
 CriticalSection.Enter;
 try
  SampleVolume:=round(NewVolume*4096);
 finally
  CriticalSection.Leave;
 end;
end;

procedure TBeRoSoundSystem.SetMixerAGC(Enabled:boolean);
begin
 CriticalSection.Enter;
 try
  AGCActive:=Enabled;
 finally
  CriticalSection.Leave;
 end;
end;

procedure TBeRoSoundSystem.FillBuffer;
type pbyte=^byte;
     psmallint=^smallint;
     plongint=^longint;
var i,jl,jr:integer;
    pl,pll,plr:plongint;
    ps:psmallint;
    pb:pbyte;
begin
 CriticalSection.Enter;
 try
  // Clearing
  FillChar(MixingBuffer^,MixingBufferSize,#0);

  // Mixing all XM tracks
  for i:=low(TBSSXMTracks) to high(TBSSXMTracks) do begin
   if assigned(XMTracks[i]) and XMTracks[i].Playing then begin
    XMTracks[i].FillBuffer;
    XMTracks[i].MixTo(MixingBuffer,MusicVolume);
   end;
  end;

  // Mixing all samples
  for i:=low(TBSSSamples) to high(TBSSSamples) do begin
   if assigned(Samples[i]) and assigned(Samples[i].Data) then begin
    Samples[i].MixTo(MixingBuffer,SampleVolume);
   end;
  end;

  // Apply master volume
  if MasterVolume<>4096 then begin
   pl:=pointer(MixingBuffer);
 {$ifdef UnrolledLoops}
   for i:=1 to BufferChannelSamples shr 2 do begin
    pl^:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(pl^*MasterVolume,12);;
    inc(pl);
    pl^:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(pl^*MasterVolume,12);;
    inc(pl);
    pl^:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(pl^*MasterVolume,12);;
    inc(pl);
    pl^:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(pl^*MasterVolume,12);;
    inc(pl);
   end;
   for i:=1 to BufferChannelSamples and 3 do begin
    pl^:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(pl^*MasterVolume,12);;
    inc(pl);
   end;
 {$else}
   for i:=1 to BufferChannelSamples do begin
    pl^:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(pl^*MasterVolume,12);;
    inc(pl);
   end;
 {$endif}
  end;

  if AGCActive then begin
  // Automatic gain control
   pl:=pointer(MixingBuffer);
   for i:=1 to BufferSamples do begin
    jl:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(pl^*AGC,8);
    pl^:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(longint((abs(jl+32768)-1)-abs(jl-32767)),1);
    inc(pl);
    jr:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(pl^*AGC,8);
    pl^:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(longint((abs(jr+32768)-1)-abs(jr-32767)),1);
    inc(pl);
    if ((jl<-32768) or (jl>32767)) or ((jr<-32768) or (jr>32767)) then begin
     dec(AGC);
     AGCCounter:=0;
    end else begin
     if AGC<256 then begin
      inc(AGCCounter);
      if AGCCounter>=AGCInterval then begin
       AGCCounter:=0;
       inc(AGC);
      end;
     end;
    end;
   end;
  end else begin
   // Clipping (condition-less!)
   pl:=pointer(MixingBuffer);
 {$ifdef UnrolledLoops}
   for i:=1 to BufferChannelSamples shr 2 do begin
    pl^:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(longint((abs(pl^+32768)-1)-abs(pl^-32767)),1);;
    inc(pl);
    pl^:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(longint((abs(pl^+32768)-1)-abs(pl^-32767)),1);;
    inc(pl);
    pl^:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(longint((abs(pl^+32768)-1)-abs(pl^-32767)),1);;
    inc(pl);
    pl^:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(longint((abs(pl^+32768)-1)-abs(pl^-32767)),1);;
    inc(pl);
   end;
   for i:=1 to BufferChannelSamples and 3 do begin
    pl^:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(longint((abs(pl^+32768)-1)-abs(pl^-32767)),1);;
    inc(pl);
   end;
 {$else}
   for i:=1 to BufferChannelSamples do begin
    pl^:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(longint((abs(pl^+32768)-1)-abs(pl^-32767)),1);;
    inc(pl);
   end;
 {$endif}
  end;

  // Downmixing
  if Channels=1 then begin
   pl:=pointer(MixingBuffer);
   pll:=pl;
   plr:=pl;
   inc(plr);
 {$ifdef UnrolledLoops}
   for i:=1 to BufferSamples shr 3 do begin
    pl^:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(pll^+plr^,1);;
    inc(pl);
    inc(pll,2);
    inc(plr,2);
    pl^:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(pll^+plr^,1);;
    inc(pl);
    inc(pll,2);
    inc(plr,2);
    pl^:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(pll^+plr^,1);;
    inc(pl);
    inc(pll,2);
    inc(plr,2);
    pl^:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(pll^+plr^,1);;
    inc(pl);
    inc(pll,2);
    inc(plr,2);
   end;
   for i:=1 to BufferSamples and 3 do begin
    pl^:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(pll^+plr^,1);;
    inc(pl);
    inc(pll,2);
    inc(plr,2);
   end;
 {$else}
   for i:=1 to BufferSamples do begin
    pl^:={$ifdef HasSAR}SARLongint{$else}SAR{$endif}(pll^+plr^,1);;
    inc(pl);
    inc(pll,2);
    inc(plr,2);
   end;
 {$endif}
  end;
  case Bits of
   8:begin
    pb:=pointer(OutputBuffer);
    pl:=pointer(MixingBuffer);
 {$ifdef UnrolledLoops}
    for i:=1 to BufferOutputChannelSamples shr 2 do begin
     pb^:=(pl^+32768) shr 8;
     inc(pb);
     inc(pl);
     pb^:=(pl^+32768) shr 8;
     inc(pb);
     inc(pl);
     pb^:=(pl^+32768) shr 8;
     inc(pb);
     inc(pl);
     pb^:=(pl^+32768) shr 8;
     inc(pb);
     inc(pl);
    end;
    for i:=1 to BufferOutputChannelSamples and 3 do begin
     pb^:=(pl^+32768) shr 8;
     inc(pb);
     inc(pl);
    end;
 {$else}
    for i:=1 to BufferOutputChannelSamples do begin
     pb^:=(pl^+32768) shr 8;
     inc(pb);
     inc(pl);
    end;
 {$endif}
   end;
   16:begin
    ps:=pointer(OutputBuffer);
    pl:=pointer(MixingBuffer);
 {$ifdef UnrolledLoops}
    for i:=1 to BufferOutputChannelSamples shr 2 do begin
     ps^:=pl^;
     inc(ps);
     inc(pl);
     ps^:=pl^;
     inc(ps);
     inc(pl);
     ps^:=pl^;
     inc(ps);
     inc(pl);
     ps^:=pl^;
     inc(ps);
     inc(pl);
    end;
    for i:=1 to BufferOutputChannelSamples and 3 do begin
     ps^:=pl^;
     inc(ps);
     inc(pl);
    end;
 {$else}
    for i:=1 to BufferOutputChannelSamples do begin
     ps^:=pl^;
     inc(ps);
     inc(pl);
    end;
 {$endif}
   end;
  end;
 finally
  CriticalSection.Leave;
 end;
end;

function TBeRoSoundSystem.LoadSample(SampleSlotNumber:byte;DataPtr:pointer;DataLen,Polyphony:integer):boolean;
var Daten:TBeRoXMMemoryStream;
    WSMPOffset:longword;
    DestSample:TBSSSample;
 function LoadWAV:boolean;
 type TWaveFileHeader=packed record
       Signatur:array[1..4] of char;
       Groesse:longword;
       WAVESignatur:array[1..4] of char;
      end;
      TWaveFormatHeader=packed record
       FormatTag:word;
       Kaenale:word;
       SamplesProSekunde:longword;
       AvgBytesProSekunde:longword;
       SampleGroesse:word;
       BitsProSample:word;
      end;
      TWaveSampleHeader=packed record
       Manufacturer:longword;
       Produkt:longword;
       SamplePeriode:longword;
       BasisNote:longword;
       PitchFraction:longword;
       SMTPEFormat:longword;
       SMTPEOffset:longword;
       SampleLoops:longword;
       SamplerData:longword;
      end;
      TWaveSampleLoopHeader=packed record
       Identifier:longword;
       SchleifenTyp:longword;
       SchleifeStart:longword;
       SchleifeEnde:longword;
       Fraction:longword;
       AnzahlSpielen:longword;
      end;
      TWaveInfoHeader=array[1..4] of char;
      TWaveXtraHeader=packed record
       Flags:longword;
       Pan:word;
       Lautstaerke:word;
       GlobalLautstaerke:word;
       Reserviert:word;
       VibType:byte;
       VibSweep:byte;
       VibDepth:byte;
       VibRate:byte;
      end;
      TWaveChunkHeader=packed record
       Signatur:array[1..4] of char;
       Groesse:longword;
      end;
      PSample24Value=^TSample24Value;
      TSample24Value=packed record
       A,B,C:byte;
      end;
 const IMAADPCMUnpackTable:array[0..88] of word=(
        7,         8,     9,    10,    11,    12,    13,    14,
        16,       17,    19,    21,    23,    25,    28,    31,
        34,       37,    41,    45,    50,    55,    60,    66,
        73,       80,    88,    97,   107,   118,   130,   143,
        157,     173,   190,   209,   230,   253,   279,   307,
        337,     371,   408,   449,   494,   544,   598,   658,
        724,     796,   876,   963,  1060,  1166,  1282,  1411,
        1552,   1707,  1878,  2066,  2272,  2499,  2749,  3024,
        3327,   3660,  4026,  4428,  4871,  5358,  5894,  6484,
        7132,   7845,  8630,  9493, 10442, 11487, 12635, 13899,
        15289, 16818, 18500, 20350, 22385, 24623, 27086, 29794,
        32767);
        IMAADPCMIndexTable:array[0..7] of shortint=(-1,-1,-1,-1,2,4,6,8);
 var Header:TWaveFileHeader;
     WaveFormatHeader:TWaveFormatHeader;
     WaveFormatHeaderPCM:TWaveFormatHeader;
     WaveFormatHeaderTemp:TWaveFormatHeader;
     WaveSampleHeader:TWaveSampleHeader;
     WaveSampleLoopHeader:TWaveSampleLoopHeader;
     WaveInfoHeader:TWaveInfoHeader;
     WaveXtraHeader:TWaveXtraHeader;
     WaveChunkHeader:TWaveChunkHeader;
     WaveFormatHeaderDa:boolean;
     WaveFormatHeaderPCMDa:boolean;
     Fact:longword;
     Data:longword;
     Smpl:longword;
     Xtra:longword;
     List:longword;
     Next:integer;
     SampleGroesse:longword;
     PB:pbyte;
     PW:pword;
     PDW:plongword;
     I:integer;
     Groesse,ADPCMLength:integer;
     ADPCMPointer,ADPCMWorkPointer:pbyte;
     ADPCMCode,ADPCMDiff,ADPCMPredictor,ADPCMStepIndex:integer;
     ADPCMStep:longword;
     Bits,Kaenale:longword;
     FloatingPoint:boolean;
     Laenge,LaengeEx,SampleRate:longword;
     Panning:boolean;
     DataPointer:pointer;
     RealSize:integer;
     SchleifeStart:longword;
     SchleifeEnde:longword;
     SustainSchleifeStart:longword;
     SustainSchleifeEnde:longword;
     Pan,Lautstaerke,GlobalLautstaerke:longword;
     Counter,EndValue:integer;
     LW32:longword;
     L32:longint absolute LW32;
     S8:pshortint;
     S16:psmallint;
     S24:PSample24Value;
     S32:plongint;
     S32F:psingle;
     S32Out:plongint;
     SampleData:pointer;
     SampleDataSize:longword;
     ItemNr:integer;
     LoopType:byte;
     SustainLoopType:byte;
 begin
  result:=false;
  Daten.Seek(0);
  if Daten.read(Header,sizeof(TWaveFileHeader))<>sizeof(TWaveFileHeader) then begin
   exit;
  end;
  if (Header.Signatur<>'RIFF') and (Header.Signatur<>'LIST') then begin
   exit;
  end;
  if (Header.WAVESignatur<>'WAVE') and (Header.WAVESignatur<>'wave') then begin
   exit;
  end;
 //IF ASSIGNED(WSMPOffset) THEN WSMPOffset^:=0;
  FILLCHAR(WaveFormatHeader,sizeof(TWaveFormatHeader),#0);
  FILLCHAR(WaveFormatHeaderPCM,sizeof(TWaveFormatHeader),#0);
  WaveFormatHeaderDa:=false;
  WaveFormatHeaderPCMDa:=false;
  Fact:=0;
  Data:=0;
  Smpl:=0;
  Xtra:=0;
  List:=0;
  LoopType:=BSSSoundLoopModeNONE;
  SustainLoopType:=BSSSoundLoopModeNONE;
  while (Daten.Position+8)<Daten.Size do begin
   if Daten.read(WaveChunkHeader,sizeof(TWaveChunkHeader))<>sizeof(TWaveChunkHeader) then begin
    result:=false;
    exit;
   end;
   Next:=Daten.Position+integer(WaveChunkHeader.Groesse);
   if (WaveChunkHeader.Signatur='fmt ') or (WaveChunkHeader.Signatur='fmt'#0) then begin
    if not WaveFormatHeaderDa then begin
     WaveFormatHeaderDa:=true;
     if Daten.read(WaveFormatHeader,sizeof(TWaveFormatHeader))<>sizeof(TWaveFormatHeader) then begin
      result:=false;
      exit;
     end;
    end;
   end else if (WaveChunkHeader.Signatur='pcm ') or (WaveChunkHeader.Signatur='pcm'#0) then begin
    if not WaveFormatHeaderPCMDa then begin
     WaveFormatHeaderPCMDa:=true;
     if Daten.read(WaveFormatHeaderPCM,sizeof(TWaveFormatHeader))<>sizeof(TWaveFormatHeader) then begin
      result:=false;
      exit;
     end;
    end;
   end else if WaveChunkHeader.Signatur='fact' then begin
    if Fact=0 then begin
     if Daten.read(Fact,4)<>4 then begin
      result:=false;
      exit;
     end;
    end;
   end else if WaveChunkHeader.Signatur='data' then begin
    if Data=0 then begin
     Data:=Daten.Position-sizeof(TWaveChunkHeader);
    end;
   end else if WaveChunkHeader.Signatur='smpl' then begin
    if Smpl=0 then begin
     Smpl:=Daten.Position-sizeof(TWaveChunkHeader);
    end;
   end else if WaveChunkHeader.Signatur='xtra' then begin
    if Xtra=0 then begin
     Xtra:=Daten.Position-sizeof(TWaveChunkHeader);
    end;
   end else if WaveChunkHeader.Signatur='list' then begin
    if List=0 then begin
     List:=Daten.Position-sizeof(TWaveChunkHeader);
    end;
   end else if WaveChunkHeader.Signatur='wsmp' then begin
    WSMPOffset:=Daten.Position;
   end;
   Daten.Seek(Next);
  end;
  if WaveFormatHeaderDa and WaveFormatHeaderPCMDa then begin
   if (SwapWordLittleEndian(WaveFormatHeader.FormatTag)<>1) and (SwapWordLittleEndian(WaveFormatHeader.FormatTag)<>3) then begin
    WaveFormatHeaderTemp:=WaveFormatHeader;
    WaveFormatHeader:=WaveFormatHeaderPCM;
    WaveFormatHeaderPCM:=WaveFormatHeaderTemp;
   end else begin
 // WaveFormatHeaderPCMDa:=FALSE;
   end;
  end;
  if (SwapWordLittleEndian(WaveFormatHeader.FormatTag)=1) or (SwapWordLittleEndian(WaveFormatHeader.FormatTag)=3) or (SwapWordLittleEndian(WaveFormatHeader.FormatTag)=$fffe) then begin
   if (SwapWordLittleEndian(WaveFormatHeader.Kaenale)<>1) and (SwapWordLittleEndian(WaveFormatHeader.Kaenale)<>2) then begin
    result:=false;
    exit;
   end;
   if (SwapWordLittleEndian(WaveFormatHeader.BitsProSample)<>8) and (SwapWordLittleEndian(WaveFormatHeader.BitsProSample)<>16) and (SwapWordLittleEndian(WaveFormatHeader.BitsProSample)<>24) and (SwapWordLittleEndian(WaveFormatHeader.BitsProSample)<>32) then begin
    result:=false;
    exit;
   end;
  end else if SwapWordLittleEndian(WaveFormatHeader.FormatTag)=17 then begin
   if SwapWordLittleEndian(WaveFormatHeader.Kaenale)<>1 then begin
    result:=false;
    exit;
   end;
   if SwapWordLittleEndian(WaveFormatHeader.BitsProSample)<>4 then begin
    result:=false;
    exit;
   end;
  end else begin
   result:=false;
   exit;
  end;
  if Data=0 then begin
   result:=false;
   exit;
  end;
  Daten.Seek(Data);
  if Daten.read(WaveChunkHeader,sizeof(TWaveChunkHeader))<>sizeof(TWaveChunkHeader) then begin
   result:=false;
   exit;
  end;
  Bits:=SwapWordLittleEndian(WaveFormatHeader.BitsProSample);
  Kaenale:=SwapWordLittleEndian(WaveFormatHeader.Kaenale);
  FloatingPoint:=WaveFormatHeader.FormatTag=3;
  if SwapWordLittleEndian(WaveFormatHeader.FormatTag)=17 then begin
   SampleGroesse:=1;
   Laenge:=(((SwapDWordLittleEndian(WaveChunkHeader.Groesse)-4)*2)+1) div SampleGroesse;
   LaengeEx:=Laenge;
   FloatingPoint:=false;
  end else begin
   SampleGroesse:=(Kaenale*(Bits shr 3));
   Laenge:=SwapDWordLittleEndian(WaveChunkHeader.Groesse) div SampleGroesse;
   LaengeEx:=SwapDWordLittleEndian(WaveChunkHeader.Groesse) div (Bits shr 3);
  end;
  SampleRate:=SwapDWordLittleEndian(WaveFormatHeader.SamplesProSekunde);
  Panning:=false;
  case SwapWordLittleEndian(WaveFormatHeader.FormatTag) of
   1,3,$fffe:begin
    GETMEM(DataPointer,SwapDWordLittleEndian(WaveChunkHeader.Groesse));
    if Daten.read(DataPointer^,SwapDWordLittleEndian(WaveChunkHeader.Groesse))<>integer(SwapDWordLittleEndian(WaveChunkHeader.Groesse)) then begin
     result:=false;
     exit;
    end;
    RealSize:=WaveChunkHeader.Groesse;
    if (Bits=8) and (RealSize>0) then begin
     PB:=DataPointer;
     for I:=1 to RealSize do begin
      PB^:=PB^ xor $80;
      inc(PB);
     end;
    end;
    if (Bits=16) and (RealSize>0) then begin
     PW:=DataPointer;
     for I:=1 to RealSize do begin
      SwapLittleEndianData16(PW^);
      inc(PW);
     end;
    end;
    if (Bits=32) and (RealSize>0) then begin
     PDW:=DataPointer;
     for I:=1 to RealSize do begin
      SwapLittleEndianData32(PDW^);
      inc(PDW);
     end;
    end;
   end;
   17:begin
    Bits:=16;
    ADPCMLength:=SwapDWordLittleEndian(WaveChunkHeader.Groesse);
    RealSize:=((ADPCMLength-4)*4)+1;
    GETMEM(DataPointer,RealSize);
    GETMEM(ADPCMPointer,ADPCMLength);
    if Daten.read(ADPCMPointer^,ADPCMLength)<>ADPCMLength then begin
     result:=false;
     exit;
    end;
    ADPCMWorkPointer:=ADPCMPointer;
    ADPCMPredictor:=psmallint(ADPCMWorkPointer)^;
    psmallint(DataPointer)^:=ADPCMPredictor;
    ADPCMStepIndex:=pbyte(longword(longword(ADPCMWorkPointer)+2))^;
    inc(ADPCMWorkPointer,4);
    ADPCMLength:=(ADPCMLength-4) shl 1;
    for I:=0 to ADPCMLength-1 do begin
     ADPCMCode:=pbyte(longword(longword(ADPCMWorkPointer)+longword(I shr 1)))^;
     ADPCMCode:=(ADPCMCode shr ((I and 1) shl 2)) and $f;
     ADPCMStep:=IMAADPCMUnpackTable[ADPCMStepIndex];
     ADPCMDiff:=ADPCMStep shr 3;
     if (ADPCMCode and 1)<>0 then inc(ADPCMDiff,ADPCMStep shr 2);
     if (ADPCMCode and 2)<>0 then inc(ADPCMDiff,ADPCMStep shr 1);
     if (ADPCMCode and 4)<>0 then inc(ADPCMDiff,ADPCMStep);
     if (ADPCMCode and 8)<>0 then ADPCMDiff:=-ADPCMDiff;
     inc(ADPCMPredictor,ADPCMDiff);
     if ADPCMPredictor<-$8000 then begin
      ADPCMPredictor:=-$8000;
     end else if ADPCMPredictor>$7fff then begin
      ADPCMPredictor:=$7fff;
     end;
     psmallint(ptruint(ptruint(DataPointer)+longword((I+1)*sizeof(smallint))))^:=ADPCMPredictor;
     inc(ADPCMStepIndex,IMAADPCMIndexTable[ADPCMCode and 7]);
     if ADPCMStepIndex<0 then begin
      ADPCMStepIndex:=0;
     end else if ADPCMStepIndex>88 then begin
      ADPCMStepIndex:=88;
     end;
    end;
    FREEMEM(ADPCMPointer);
   end;
   else begin
 // DataPointer:=NIL;
    result:=false;
    exit;
   end;
  end;
  if Smpl<>0 then begin
   Daten.Seek(Smpl);
   if Daten.read(WaveChunkHeader,sizeof(TWaveChunkHeader))<>sizeof(TWaveChunkHeader) then begin
    result:=false;
    exit;
   end;
   if Daten.read(WaveSampleHeader,sizeof(TWaveSampleHeader))<>sizeof(TWaveSampleHeader) then begin
    result:=false;
    exit;
   end;
   if SwapDWordLittleEndian(WaveSampleHeader.SampleLoops)>1 then begin
    if Daten.read(WaveSampleLoopHeader,sizeof(TWaveSampleLoopHeader))<>sizeof(TWaveSampleLoopHeader) then begin
     result:=false;
     exit;
    end;
    case WaveSampleLoopHeader.SchleifenTyp of
     1:SustainLoopType:=BSSSoundLoopModePINGPONG;
     2:SustainLoopType:=BSSSoundLoopModeBACKWARD;
     else SustainLoopType:=BSSSoundLoopModeFORWARD;
    end;
    SustainSchleifeStart:=SwapDWordLittleEndian(WaveSampleLoopHeader.SchleifeStart);
    SustainSchleifeEnde:=SwapDWordLittleEndian(WaveSampleLoopHeader.SchleifeEnde);
    if SustainSchleifeStart>=SustainSchleifeEnde then begin
     SustainLoopType:=BSSSoundLoopModeNONE;
    end;
   end;
   if SwapDWordLittleEndian(WaveSampleHeader.SampleLoops)>0 then begin
    if Daten.read(WaveSampleLoopHeader,sizeof(TWaveSampleLoopHeader))<>sizeof(TWaveSampleLoopHeader) then begin
     result:=false;
     exit;
    end;
    case WaveSampleLoopHeader.SchleifenTyp of
     1:LoopType:=BSSSoundLoopModePINGPONG;
     2:LoopType:=BSSSoundLoopModeBACKWARD;
     else LoopType:=BSSSoundLoopModeFORWARD;
    end;
    SchleifeStart:=SwapDWordLittleEndian(WaveSampleLoopHeader.SchleifeStart);
    SchleifeEnde:=SwapDWordLittleEndian(WaveSampleLoopHeader.SchleifeEnde);
    if SchleifeStart>=SchleifeEnde then begin
     LoopType:=BSSSoundLoopModeNONE;
    end;
   end;
  end;
  if List<>0 then begin
   Daten.Seek(List);
   if Daten.read(WaveChunkHeader,sizeof(TWaveChunkHeader))<>sizeof(TWaveChunkHeader) then begin
    result:=false;
    exit;
   end;
   if Daten.read(WaveInfoHeader,sizeof(TWaveInfoHeader))<>sizeof(TWaveInfoHeader) then begin
    result:=false;
    exit;
   end;
   if WaveInfoHeader='INFO' then begin
    Groesse:=Daten.Position+integer(SwapDWordLittleEndian(WaveChunkHeader.Groesse));
    while (Daten.Position+8)<Groesse do begin
     if Daten.read(WaveChunkHeader,sizeof(TWaveChunkHeader))<>sizeof(TWaveChunkHeader) then begin
      result:=false;
      exit;
     end;
     Next:=Daten.Position+integer(SwapDWordLittleEndian(WaveChunkHeader.Groesse));
     Daten.Seek(Next);
    end;
   end;
  end;
  if Xtra<>0 then begin
   Daten.Seek(Xtra);
   if Daten.read(WaveChunkHeader,sizeof(TWaveChunkHeader))<>sizeof(TWaveChunkHeader) then begin
    result:=false;
    exit;
   end;
   if Daten.read(WaveXtraHeader,sizeof(TWaveXtraHeader))<>sizeof(TWaveXtraHeader) then begin
    result:=false;
    exit;
   end;
   SwapLittleEndianData32(WaveXtraHeader.Flags);
   SwapLittleEndianData16(WaveXtraHeader.Pan);
   SwapLittleEndianData16(WaveXtraHeader.Lautstaerke);
   SwapLittleEndianData16(WaveXtraHeader.GlobalLautstaerke);
   SwapLittleEndianData16(WaveXtraHeader.Reserviert);
   Pan:=WaveXtraHeader.Pan;
   Lautstaerke:=WaveXtraHeader.Lautstaerke;
   GlobalLautstaerke:=WaveXtraHeader.GlobalLautstaerke;
  end;
  if assigned(DataPointer) then begin
   SampleDataSize:=Laenge*2*sizeof(longint);
   GETMEM(SampleData,SampleDataSize);
   S32Out:=SampleData;
   case Kaenale of
    1:begin
     EndValue:=Laenge;
     case Bits of
      8:begin
       S8:=DataPointer;
       for Counter:=1 to EndValue do begin
        S32Out^:=S8^ shl 8;
        inc(S32Out);
        S32Out^:=S8^ shl 8;
        inc(S32Out);
        inc(S8);
       end;
      end;
      16:begin
       S16:=DataPointer;
       for Counter:=1 to EndValue do begin
        S32Out^:=S16^;
        inc(S32Out);
        S32Out^:=S16^;
        inc(S32Out);
        inc(S16);
       end;
      end;
      24:begin
       S24:=DataPointer;
       for Counter:=1 to EndValue do begin
        LW32:=(S24^.A shl 8) or (S24^.B shl 16) or (S24^.C shl 24);
        S32Out^:=SAR(L32,8);
        inc(S32Out);
        S32Out^:=SAR(L32,8);
        inc(S32Out);
        inc(S24);
       end;
      end;
      32:begin
       if FloatingPoint then begin
        S32F:=DataPointer;
        for Counter:=1 to EndValue do begin
         S32Out^:=round(S32F^*32767);
         inc(S32Out);
         S32Out^:=round(S32F^*32767);
         inc(S32Out);
         inc(S32F);
        end;
       end else begin
        S32:=DataPointer;
        for Counter:=1 to EndValue do begin
         S32Out^:=sar(S32^,16);
         inc(S32Out);
         S32Out^:=sar(S32^,16);
         inc(S32Out);
         inc(S32);
        end;
       end;
      end;
     end;
    end;
    2:begin
     EndValue:=Laenge*2;
     case Bits of
      8:begin
       S8:=DataPointer;
       for Counter:=1 to EndValue do begin
        S32Out^:=S8^ shl 8;
        inc(S8);
        inc(S32Out);
       end;
      end;
      16:begin
       S16:=DataPointer;
       for Counter:=1 to EndValue do begin
        S32Out^:=S16^;
        inc(S16);
        inc(S32Out);
       end;
      end;
      24:begin
       S24:=DataPointer;
       for Counter:=1 to EndValue do begin
        LW32:=(S24^.A shl 8) or (S24^.B shl 16) or (S24^.C shl 24);
        S32Out^:=SAR(L32,8);
        inc(S24);
        inc(S32Out);
       end;
      end;
      32:begin
       if FloatingPoint then begin
        S32F:=DataPointer;
        for Counter:=1 to EndValue do begin
         S32Out^:=round(S32F^*32767);
         inc(S32F);
         inc(S32Out);
        end;
       end else begin
        S32:=DataPointer;
        for Counter:=1 to EndValue do begin
         S32Out^:=sar(S32^,16);
         inc(S32);
         inc(S32Out);
        end;
       end;
      end;
     end;
    end;
   end;
   try
    DestSample.SampleLength:=Laenge;
    DestSample.SampleRate:=SampleRate;
    DestSample.Loop.Mode:=LoopType;
    DestSample.Loop.StartSample:=SchleifeStart;
    DestSample.Loop.EndSample:=SchleifeEnde;
    DestSample.SustainLoop.Mode:=SustainLoopType;
    DestSample.SustainLoop.StartSample:=SustainSchleifeStart;
    DestSample.SustainLoop.EndSample:=SustainSchleifeEnde;
    GETMEM(DestSample.Data,(Laenge+(2*BSSSampleFixUp))*2*sizeof(longint));
    FillChar(DestSample.Data^,(Laenge+(2*BSSSampleFixUp))*2*sizeof(longint),#0);
    inc(plongint(DestSample.Data),2*BSSSampleFixUp);
    MOVE(SampleData^,DestSample.Data^,Laenge*2*sizeof(longint));
   finally
    FREEMEM(SampleData);
    FREEMEM(DataPointer);
   end;
  end;
  result:=true;
 end;
begin
 result:=false;
 CriticalSection.Enter;
 try
  DestSample:=Samples[SampleSlotNumber];
  if assigned(DestSample) then begin
   DestSample.Destroy;
  end;
  DestSample:=TBSSSample.Create(self);
  if assigned(DataPtr) and (DataLen>0) then begin
   DestSample.SetPolyphony(Polyphony);
   Daten:=TBeRoXMMemoryStream.Create;
   Daten.Write(DataPtr^,DataLen);
   Daten.Seek(0);
   result:=LoadWAV;
   Daten.Destroy;
  end;
  if result then begin
   DestSample.FixUp;
  end else begin
   DestSample.Destroy;
   DestSample:=nil;
  end;
  Samples[SampleSlotNumber]:=DestSample;
 finally
  CriticalSection.Leave;
 end;
end;

function TBeRoSoundSystem.UnloadSample(SampleSlotNumber:byte):boolean;
begin
 CriticalSection.Enter;
 try
  if assigned(Samples[SampleSlotNumber]) then begin
   Samples[SampleSlotNumber].Destroy;
   Samples[SampleSlotNumber]:=nil;
   result:=true;
  end else begin
   result:=false;
  end;
 finally
  CriticalSection.Leave;
 end;
end;

function TBeRoSoundSystem.PlaySample(SampleSlotNumber:byte;Volume,Panning,Rate:single):integer;
begin
 CriticalSection.Enter;
 try
  if assigned(Samples[SampleSlotNumber]) then begin
   result:=Samples[SampleSlotNumber].Play(round(Volume*65536.0),round(Panning*65536.0),round(Rate*65536.0));
  end else begin
   result:=-1;
  end;
 finally
  CriticalSection.Leave;
 end;
end;

function TBeRoSoundSystem.IsSamplePlaying(SampleSlotNumber:byte):boolean;
var i:integer;
begin
 result:=false;
 CriticalSection.Enter;
 try
  if assigned(Samples[SampleSlotNumber]) then begin
   for i:=0 to length(Samples[SampleSlotNumber].Voices)-1 do begin
    if Samples[SampleSlotNumber].Voices[i].Active then begin
     result:=true;
     break;
    end;
   end;
  end;
 finally
  CriticalSection.Leave;
 end;
end;

function TBeRoSoundSystem.IsSampleVoicePlaying(SampleSlotNumber:byte;VoiceNumber:integer):boolean;
var i:integer;
begin
 result:=false;
 CriticalSection.Enter;
 try
  if assigned(Samples[SampleSlotNumber]) then begin
   i:=VoiceNumber;
   if (i>=0) and (i<length(Samples[SampleSlotNumber].Voices)) then begin
    if Samples[SampleSlotNumber].Voices[i].Active then begin
     result:=true;
    end;
   end;
  end;
 finally
  CriticalSection.Leave;
 end;
end;

procedure TBeRoSoundSystem.StopSample(SampleSlotNumber:byte;VoiceNumber:integer);
begin
 CriticalSection.Enter;
 try
  if assigned(Samples[SampleSlotNumber]) then begin
   Samples[SampleSlotNumber].Stop(VoiceNumber);
  end;
 finally
  CriticalSection.Leave;
 end;
end;

procedure TBeRoSoundSystem.KeyOffSample(SampleSlotNumber:byte;VoiceNumber:integer);
begin
 CriticalSection.Enter;
 try
  if assigned(Samples[SampleSlotNumber]) then begin
   Samples[SampleSlotNumber].KeyOff(VoiceNumber);
  end;
 finally
  CriticalSection.Leave;
 end;
end;

procedure TBeRoSoundSystem.SetSampleVolume(SampleSlotNumber:byte;VoiceNumber:integer;Volume:single);
begin
 CriticalSection.Enter;
 try
  if assigned(Samples[SampleSlotNumber]) then begin
   Samples[SampleSlotNumber].SetVolume(VoiceNumber,round(Volume*65536));
  end;
 finally
  CriticalSection.Leave;
 end;
end;

procedure TBeRoSoundSystem.SetSamplePanning(SampleSlotNumber:byte;VoiceNumber:integer;Panning:single);
begin
 CriticalSection.Enter;
 try
  if assigned(Samples[SampleSlotNumber]) then begin
   Samples[SampleSlotNumber].SetPanning(VoiceNumber,round(Panning*65536));
  end;
 finally
  CriticalSection.Leave;
 end;
end;

procedure TBeRoSoundSystem.SetSampleRate(SampleSlotNumber:byte;VoiceNumber:integer;Rate:single);
begin
 CriticalSection.Enter;
 try
  if assigned(Samples[SampleSlotNumber]) then begin
   Samples[SampleSlotNumber].SetRate(VoiceNumber,round(Rate*65536));
  end;
 finally
  CriticalSection.Leave;
 end;
end;

function TBeRoSoundSystem.LoadXMTrack(XMTrackSlotNumber:byte;DataPtr:pointer;DataLen:integer;Looping:boolean;Interpolation:integer=BSSXMTrackInterpolationLINEAR):boolean;
var DestXMTrack:TBeRoXM;
begin
 result:=false;
 CriticalSection.Enter;
 try
  DestXMTrack:=XMTracks[XMTrackSlotNumber];
  if assigned(DestXMTrack) then begin
   DestXMTrack.Destroy;
  end;
  DestXMTrack:=TBeRoXM.Create(SampleRate,BufferSamples,1,true,0,16,0);
  if assigned(DataPtr) and (DataLen>0) then begin
   result:=DestXMTrack.Module.Load(DataPtr,DataLen);
  end;
  if result then begin
   DestXMTrack.Module.MasterVolume:=256;
   DestXMTrack.Looping:=Looping;
   case Interpolation of
    BSSXMTrackInterpolationLINEAR:begin
     DestXMTrack.ResamplingMethod:=BeRoXMMixerLinear;
    end;
    BSSXMTrackInterpolationCUBICSPLINE:begin
     DestXMTrack.ResamplingMethod:=BeRoXMMixerCubicSpline;
    end;
    BSSXMTrackInterpolationWINDOWEDFIR:begin
     DestXMTrack.ResamplingMethod:=BeRoXMMixerWindowedFIR;
    end;
    else begin
     DestXMTrack.ResamplingMethod:=BeRoXMMixerNearest;
    end;
   end;
  end else begin
   DestXMTrack.Destroy;
   DestXMTrack:=nil;
  end;
  XMTracks[XMTrackSlotNumber]:=DestXMTrack;
 finally
  CriticalSection.Leave;
 end;
end;

function TBeRoSoundSystem.UnloadXMTrack(XMTrackSlotNumber:byte):boolean;
begin
 CriticalSection.Enter;
 try
  if assigned(XMTracks[XMTrackSlotNumber]) then begin
   XMTracks[XMTrackSlotNumber].Destroy;
   XMTracks[XMTrackSlotNumber]:=nil;
   result:=true;
  end else begin
   result:=false;
  end;
 finally
  CriticalSection.Leave;
 end;
end;

function TBeRoSoundSystem.PlayXMTrack(XMTrackSlotNumber:byte):boolean;
begin
 result:=false;
 CriticalSection.Enter;
 try
  if assigned(XMTracks[XMTrackSlotNumber]) then begin
   result:=XMTracks[XMTrackSlotNumber].Play;
  end;
 finally
  CriticalSection.Leave;
 end;
end;

function TBeRoSoundSystem.StopXMTrack(XMTrackSlotNumber:byte):boolean;
begin
 result:=false;
 CriticalSection.Enter;
 try
  if assigned(XMTracks[XMTrackSlotNumber]) then begin
   XMTracks[XMTrackSlotNumber].Stop;
   result:=true;
  end;
 finally
  CriticalSection.Leave;
 end;
end;

function TBeRoSoundSystem.IsXMTrackPlaying(XMTrackSlotNumber:byte):boolean;
begin
 result:=false;
 CriticalSection.Enter;
 try
  if assigned(XMTracks[XMTrackSlotNumber]) then begin
   result:=XMTracks[XMTrackSlotNumber].Playing;
  end;
 finally
  CriticalSection.Leave;
 end;
end;

end.
