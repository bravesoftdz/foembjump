unit BeRoMiniSoundSystem;
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
{$IFDEF FPC}
 {$MODE DELPHI}
{$ENDIF}
// BeRo Mini Sound System
// Copyright (C) 2007-2008, Benjamin 'BeRo' Rosseaux
{$IFDEF FPC}
 {$WARNINGS OFF}
 {$HINTS OFF}
 {$IFDEF CPUI386}
  {$DEFINE CPU386}
  {$ASMMODE INTEL}
 {$ENDIF}
 {$IFDEF FPC_LITTLE_ENDIAN}
  {$DEFINE LITTLE_ENDIAN}
 {$ELSE}
  {$IFDEF FPC_BIG_ENDIAN}
   {$DEFINE BIG_ENDIAN}
  {$ENDIF}
 {$ENDIF}
{$ELSE}
 {$DEFINE LITTLE_ENDIAN}
 {$IFNDEF CPU64}
  {$DEFINE CPU32}
 {$ENDIF}
 {$OPTIMIZATION ON}
{$ENDIF}
{$UNDEF caninline}
{$UNDEF canx86simd}
{$IFDEF VER180}
 {$DEFINE caninline}
 {$IFDEF CPU386}
  {$DEFINE canx86simd}
 {$ENDIF}
 {$FINITEFLOAT OFF}
{$ENDIF}
{$IFDEF FPC}
 {$DEFINE caninline}
 {$IFDEF CPU386}
  {$DEFINE canx86simd}
 {$ENDIF}
{$ELSE}
 {$BOOLEVAL OFF}
 {$SAFEDIVIDE OFF}
{$ENDIF}
{$WRITEABLECONST ON}
{$VARSTRINGCHECKS ON}
{$TYPEDADDRESS OFF}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}
{$REALCOMPATIBILITY OFF}
{$OPENSTRINGS ON}
{$LONGSTRINGS ON}
{$EXTENDEDSYNTAX ON}
{$BOOLEVAL OFF}

interface

uses Windows,MMSystem;

procedure SoundInit;
procedure SoundDone;
function SoundOpen(SampleRate,NumChannels,BitsPerSample,BufferLengthMS,PrebufferingMS:integer):boolean;
procedure SoundClose;
function SoundWrite(Buf:pointer;Len:integer):boolean;
function SoundCanWrite:integer;
function SoundIsPlaying:boolean;
function SoundPause(Pause:boolean):boolean;
procedure SoundSetVolume(Volume:integer);
procedure SoundSetPan(Pan:integer);
procedure SoundFlush(TimeMS:integer);
function SoundGetOutputTime:integer;
function SoundGetWrittenTime:integer;

implementation

const BufferSize=2048;
      WaveDevice=longword(-1);//WAVE_MAPPER;
      WriteBufferBytes=BufferSize*16;

type TWAVEHDR=WAVEHDR;

var WaveOutHandle,BufferBytes,OutputTimeOffset,
    WrittenTimeOffset,BufferVolume,BufferPanning:integer;
    BufferFilled:int64;
    WaveFormat:TWaveFormatEx;
    WaveHandler:array[0..3] of TWAVEHDR;
    Inited:boolean;
    Paused:boolean;
    IsSampleAccurate:boolean;
    LastGetPositionSample,AddGetPositionSample,FirstBufferTime:int64;
    WriteBuffer:array[0..WriteBufferBytes] of byte;
    WriteBufferSize:integer;

procedure Reset;
begin
 fillchar(WaveFormat,sizeof(WaveFormat),#0);
 fillchar(WaveHandler,sizeof(WaveHandler),#0);
 Inited:=false;
 Paused:=false;
 BufferBytes:=0;
 OutputTimeOffset:=0;
 WrittenTimeOffset:=0;
 BufferVolume:=255;
 BufferPanning:=0;
 LastGetPositionSample:=0;
 AddGetPositionSample:=0;
 FirstBufferTime:=-1;
 IsSampleAccurate:=false;
 fillchar(WriteBuffer,sizeof(WriteBuffer),#0);
 WriteBufferSize:=0;
 BufferFilled:=0;
end;

function GetOutputTime:int64;
var MMTime:TMMTime;
begin
 if IsSampleAccurate then begin
  MMTime.wType:=TIME_SAMPLES;
  if (WaveOutGetPosition(WaveOutHandle,@MMTime,sizeof(TMMTime))=MMSYSERR_NOERROR) and
     (MMTime.wType=TIME_SAMPLES) then begin
   if MMTime.sample<LastGetPositionSample then begin
    if LastGetPositionSample<=$7ffffff then begin
     inc(AddGetPositionSample,$7ffffff);
    end else begin
     inc(AddGetPositionSample,LastGetPositionSample);
     if MMTime.sample<=BufferSize then begin
      inc(AddGetPositionSample,BufferSize);
      dec(AddGetPositionSample,MMTime.sample);
     end;
    end;
   end;
   LastGetPositionSample:=MMTime.sample;
   result:=MMTime.sample+AddGetPositionSample+OutputTimeOffset;
   exit;
  end;
 end;

 MMTime.wType:=TIME_MS;
 if (WaveOutGetPosition(WaveOutHandle,@MMTime,sizeof(TMMTime))=MMSYSERR_NOERROR) and
    (MMTime.wType=TIME_MS) then begin
  result:=MMTime.ms;
  result:=((result*WaveFormat.nSamplesPerSec) div 1000)+OutputTimeOffset;
  exit;
 end;

 MMTime.wType:=TIME_BYTES;
 if (WaveOutGetPosition(WaveOutHandle,@MMTime,sizeof(TMMTime))=MMSYSERR_NOERROR) and
    (MMTime.wType=TIME_BYTES) then begin
  result:=MMTime.cb div WaveFormat.nBlockAlign;
  result:=result+OutputTimeOffset;
  exit;
 end;

 MMTime.wType:=TIME_SMPTE;
 if (WaveOutGetPosition(WaveOutHandle,@MMTime,sizeof(TMMTime))=MMSYSERR_NOERROR) and
    (MMTime.wType=TIME_SMPTE) and (MMTime.FPS<>0) then begin
  result:=(((MMTime.hour*60)+MMTime.min*60)+MMTime.sec*1000)+((MMTime.Frame*1000) div MMTime.FPS);
  result:=((result*WaveFormat.nSamplesPerSec) div 1000)+OutputTimeOffset;
  exit;
 end;

 result:=timeGetTime-FirstBufferTime;
 result:=((result*WaveFormat.nSamplesPerSec) div 1000)+OutputTimeOffset;
end;

procedure SoundInit;
begin
 Reset;
end;

procedure SoundDone;
begin
 if Inited then begin
  SoundClose;
 end;
 Reset;
end;

function SoundOpen(SampleRate,NumChannels,BitsPerSample,BufferLengthMS,PrebufferingMS:integer):boolean;
var i:integer;
    WOC:TWAVEOUTCAPS;
begin
 if Inited then begin
  result:=true;
  exit;
 end;
 if waveOutGetDevCaps(WaveDevice,@WOC,sizeof(TWAVEOUTCAPS))=0 then begin
  IsSampleAccurate:=(WOC.dwSupport and WAVECAPS_SAMPLEACCURATE)<>0;
 end else begin
  IsSampleAccurate:=false;
 end;
 Reset;
 case BitsPerSample of
  32:WaveFormat.wFormatTag:=3;
  else WaveFormat.wFormatTag:=WAVE_FORMAT_PCM;
 end;
 WaveFormat.nSamplesPerSec:=SampleRate;
 WaveFormat.nChannels:=NumChannels;
 WaveFormat.wBitsPerSample:=BitsPerSample;
 WaveFormat.nBlockAlign:=(WaveFormat.nChannels*WaveFormat.wBitsPerSample) div 8;
 WaveFormat.nAvgBytesPerSec:=WaveFormat.nSamplesPerSec*WaveFormat.nBlockAlign;
 WaveFormat.cbSize:=0;
 BufferBytes:=BufferSize*WaveFormat.nBlockAlign;
 for i:=low(WaveHandler) to high(WaveHandler) do begin
  if assigned(WaveHandler[i].lpData) then begin
   freemem(WaveHandler[i].lpData);
   WaveHandler[i].lpData:=nil;
  end;
  fillchar(WaveHandler[i],sizeof(TWAVEHDR),#0);
  WaveHandler[i].dwBufferLength:=BufferBytes;
  getmem(WaveHandler[i].lpData,WaveHandler[i].dwBufferLength);
  WaveHandler[i].dwBytesRecorded:=0;
  WaveHandler[i].dwUser:=0;
  WaveHandler[i].dwFlags:=WHDR_DONE;
  WaveHandler[i].dwLoops:=0;
 end;
 if waveOutOpen(@WaveOutHandle,WaveDevice,@WaveFormat,0,0,0)<>MMSYSERR_NOERROR then begin
  result:=false;
  exit;
 end;
 Inited:=true;
 result:=true;
end;

procedure SoundClose;
var i:integer;
    j:longword;
begin
 if not Inited then begin
  exit;
 end;
 for i:=low(WaveHandler) to high(WaveHandler) do begin
  j:=timeGetTime;
  while (waveOutUnprepareHeader(WaveOutHandle,@WaveHandler[i],SIZEOF(TWAVEHDR))=WAVERR_STILLPLAYING) and (abs(timeGetTime-j)<100) do begin
   sleep(10);
  end;
  if assigned(WaveHandler[i].lpData) then begin
   freemem(WaveHandler[i].lpData);
   WaveHandler[i].lpData:=nil;
  end;
 end;
 waveOutReset(WaveOutHandle);
 waveOutClose(WaveOutHandle);
 Reset;
end;

type pbyte=^byte;

function SoundWrite(Buf:pointer;Len:integer):boolean;
const fci256:single=1/256;
      fci65536:single=1/65536;
var i,j,k,h,v,vl,vr:integer;
    b,wb:^byte;
    ok:boolean;
    vf,vlf,vrf:single;
begin
 result:=false;
 b:=Buf;
 k:=Len;
 while k>0 do begin
  j:=WriteBufferBytes-WriteBufferSize;
  if k<j then j:=k;
  if j>0 then begin
   move(b^,WriteBuffer[WriteBufferSize],j);
   if (BufferVolume<>255) or (BufferPanning<>0) then begin
    wb:=@WriteBuffer[WriteBufferSize];
    case WaveFormat.nChannels of
     1:begin
      vl:=BufferVolume;
      if vl<0 then begin
       vl:=0;
      end else if vl>256 then begin
       vl:=256;
      end;
      case WaveFormat.wBitsPerSample of
       8:begin
        for h:=1 to j do begin
         v:=wb^-128;
         v:=(v*vl) div 256;
         v:=v+128;
         if v<0 then begin
          v:=0;
         end else if v>255 then begin
          v:=255;
         end;
         wb^:=v;
         inc(wb);
        end;
       end;
       16:begin
        for h:=1 to j div 2 do begin
         v:=smallint(pointer(wb)^);
         v:=(v*vl) div 256;
         if v<-32768 then begin
          v:=-32768;
         end else if v>32767 then begin
          v:=32767;
         end;
         smallint(pointer(wb)^):=v;
         inc(wb,2);
        end;
       end;
       32:begin
        vlf:=vl*fci256;
        for h:=1 to j div 4 do begin
         vf:=single(pointer(wb)^);
         vf:=vf*vlf;
         if vf<-1 then begin
          vf:=-1;
         end else if vf>1 then begin
          vf:=1;
         end;
         single(pointer(wb)^):=vf;
         inc(wb,4);
        end;
       end;
      end;
     end;
     2:begin
      vl:=128-BufferPanning;
      vr:=128+BufferPanning;
      if vl<0 then begin
       vl:=0;
      end else if vl>256 then begin
       vl:=256;
      end;
      if vr<0 then begin
       vr:=0;
      end else if vr>256 then begin
       vr:=256;
      end;
      vl:=vl*BufferVolume;
      vr:=vr*BufferVolume;
      if vl<0 then begin
       vl:=0;
      end else if vl>65536 then begin
       vl:=65536;
      end;
      if vr<0 then begin
       vr:=0;
      end else if vr>65536 then begin
       vr:=65536;
      end;
      case WaveFormat.wBitsPerSample of
       8:begin
        for h:=1 to j div 2 do begin
         v:=wb^-128;
         v:=(v*vl) div 65536;
         v:=v+128;
         if v<0 then begin
          v:=0;
         end else if v>255 then begin
          v:=255;
         end;
         wb^:=v;
         inc(wb);
         v:=wb^-128;
         v:=(v*vr) div 65536;
         v:=v+128;
         if v<0 then begin
          v:=0;
         end else if v>255 then begin
          v:=255;
         end;
         wb^:=v;
         inc(wb);
        end;
       end;
       16:begin
        for h:=1 to j div 4 do begin
         v:=smallint(pointer(wb)^);
         v:=(v*vl) div 65536;
         if v<-32768 then begin
          v:=-32768;
         end else if v>32767 then begin
          v:=32767;
         end;
         smallint(pointer(wb)^):=v;
         inc(wb,2);
         v:=smallint(pointer(wb)^);
         v:=(v*vr) div 65536;
         if v<-32768 then begin
          v:=-32768;
         end else if v>32767 then begin
          v:=32767;
         end;
         smallint(pointer(wb)^):=v;
         inc(wb,2);
        end;
       end;
       32:begin
        vlf:=vl*fci65536;
        vrf:=vr*fci65536;
        for h:=1 to j div 8 do begin
         vf:=single(pointer(wb)^);
         vf:=vf*vlf;
         if vf<-1 then begin
          vf:=-1;
         end else if vf>1 then begin
          vf:=1;
         end;
         single(pointer(wb)^):=vf;
         inc(wb,4);
         vf:=single(pointer(wb)^);
         vf:=vf*vrf;
         if vf<-1 then begin
          vf:=-1;
         end else if vf>1 then begin
          vf:=1;
         end;
         single(pointer(wb)^):=vf;
         inc(wb,4);
        end;
       end;
      end;
     end;
    end;
   end;
   inc(WriteBufferSize,j);
   inc(b,j);
   dec(k,j);
   result:=true;
  end;
  ok:=false;
  while (WriteBufferSize>=BufferBytes) and not ok do begin
   for i:=low(WaveHandler) to high(WaveHandler) do begin
    if (WaveHandler[i].dwFlags AND WHDR_DONE)<>0 then begin
     if waveOutUnprepareHeader(WaveOutHandle,@WaveHandler[i],SIZEOF(TWAVEHDR))<>WAVERR_STILLPLAYING then begin
      if FirstBufferTime<0 then begin
       FirstBufferTime:=timeGetTime;
      end;
      move(WriteBuffer,WaveHandler[i].lpData^,BufferBytes);
      dec(WriteBufferSize,BufferBytes);
      if WriteBufferSize>0 then begin
       move(WriteBuffer[BufferBytes],WriteBuffer[0],WriteBufferSize);
      end;
      WaveHandler[i].dwBufferLength:=BufferBytes;
      WaveHandler[i].dwFlags:=WaveHandler[i].dwFlags and not WHDR_DONE;
      waveOutPrepareHeader(WaveOutHandle,@WaveHandler[i],SIZEOF(TWAVEHDR));
      waveOutWrite(WaveOutHandle,@WaveHandler[i],SIZEOF(TWAVEHDR));
      inc(WrittenTimeOffset,Len div WaveFormat.nBlockAlign);
      inc(BufferFilled);
      ok:=true;
     end;
    end;
    if ok then begin
     break;
    end else if BufferFilled>length(WaveHandler) then begin
     h:=integer(WaveFormat.nSamplesPerSec)*length(WaveHandler);
     h:=((BufferSize*1000)+(h div 2)) div h;
     if h>10 then begin
      h:=10;
     end;
     sleep(h);
    end;
   end;
  end;
 end;
end;

function SoundCanWrite:integer;
begin
 if Paused then begin
  result:=0;
 end else begin
  result:=BufferBytes;
 end;
end;

function SoundIsPlaying:boolean;
begin
 result:=Inited;
end;

function SoundPause(Pause:boolean):boolean;
begin
 if Paused then begin
  result:=true;
 end else begin
  result:=false;
 end;
 if Pause then begin
  if Inited and not Paused then begin
   waveOutPause(WaveOutHandle);
  end;
  Paused:=true;
 end else begin
  if Inited and Paused then begin
   waveOutRestart(WaveOutHandle);
  end;
  Paused:=false;
 end;
end;

procedure SoundSetVolume(Volume:integer);
begin
 if (Volume<0) or (Volume>255) then begin
  Volume:=255;
 end;
 BufferVolume:=Volume;
end;

procedure SoundSetPan(Pan:integer);
begin
 if Pan<-128 then begin
  Pan:=-128;
 end else if Pan>128 then begin
  Pan:=128;
 end;
 BufferPanning:=Pan;
end;

procedure SoundFlush(TimeMS:integer);
var v:int64;
begin
 if TimeMS<0 then begin
  exit;
 end;
 if Inited and Paused then begin
  waveOutRestart(WaveOutHandle);
  Paused:=false;
 end;
 if Inited then begin
  waveOutReset(WaveOutHandle);
 end;
 v:=TimeMS;
 v:=(v*WaveFormat.nSamplesPerSec) div 1000;
 OutputTimeOffset:=v;
 WrittenTimeOffset:=v;
end;

function SoundGetOutputTime:integer;
var v:int64;
begin
 v:=GetOutputTime;
 v:=(v*1000) div WaveFormat.nSamplesPerSec;
 result:=v;
end;

function SoundGetWrittenTime:integer; 
var v:int64;
begin
 v:=WrittenTimeOffset;
 v:=(v*1000) div WaveFormat.nSamplesPerSec;
 result:=v;
end;

initialization
 Reset;
end.
