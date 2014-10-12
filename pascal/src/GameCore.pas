unit GameCore;
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
 {$h+}
{$endif}

interface

uses SysUtils,Classes,Math,{$ifdef mobile}gles20{$else}GL,GLExt{$endif},RenderBase,BeRoVectorCanvas,BeRoSoundSystem;

const CountClouds=10;
      CountPlatforms=10;///10;
      CountFires=16;

      DeltaFactor=65536;

      EPSILON=1e-4;

      MaxCloudScale=1.5;

      ScreenHz=60;
      DeltaSpeedFactor=1000/(1000/ScreenHz);
      ScreenInvHzEx=(1000*DeltaFactor) div ScreenHz;
      ScreenInvHz=ScreenInvHzEx/(1000*DeltaFactor);
      InvDelta=1/(1000*DeltaFactor);

      MonsterTopOfsY=0;

      MonsterYOfs=32;

      SpringTopOfsY=10;

      PlatformTopOfsY=11;

      SpringOfsY=-(35);

      MaxSpringYOfs=23;

      PlayerSpringOfsY=20;

      PlayerSubYHeadRoom=32;

      psNONE=0;
      psJUMPING=1;
      psFALLING=2;
      psSPRING=3;
      psDEAD=4;

      LastBestResolution:integer=-1;

      SoundRunning:boolean=false;

      FirstFrameWasDrawn:boolean=false;

      sTITLESCREEN=0;
      sPRESTARTSCREEN=1;
      sGAME=2;
      sPOSTGAMESCREEN=3;

      ssNONE=0;
      ssDOWN=1;
      ssUP=2;

type TFontCharWidths=array[ansichar] of word;

{$ifndef fpc}
     ptrint=longint;
     ptruint=longword;
{$endif}

     TCloud=record
      x,y,w,h,a,s:single;
      k:integer;
     end;

     TClouds=array[0..CountClouds-1] of TCloud;

     TCircleOrder=array[0..CountClouds-1] of integer;

     TFire=record
      Active:boolean;
      x,y,w,h,dx,dy,o:single;
      CollisionMap:PCollisionMap;
     end;

     TFires=array[0..CountFires-1] of TFire;

     TPlatform=record
      x,y,Width,Height,Direction:single;
      IsMoving,Touched,TouchedFromDown,SpringTouched,MonsterTouched,Spring,Monster,Star,MonsterDead:boolean;
      Kind:integer;
      CollisionMap:PCollisionMap;
      SpringX,SpringY,SpringYOfs,SpringYSpeed,SpringWidth,SpringHeight,MonsterDeadTime:single;
      SpringState:integer;
      SpringCollisionMap:PCollisionMap;
      StarX,StarY,StarWidth,StarHeight:single;
      StarCollisionMap:PCollisionMap;
      MonsterDirection,MonsterFallSpeed,MonsterXOfs,MonsterYOfs,MonsterX,MonsterX2,MonsterY,MonsterWidth,MonsterHeight:single;
      MonsterCollisionMap:PCollisionMap;
      Visible,InRange,SpringInRange,MonsterInRange,StarInRange:boolean;
      Collision,SpringCollision,MonsterCollision,MonsterFireCollision,StarCollision:array[boolean] of boolean;
      MonsterFireCollisionNo:array[boolean] of integer;
     end;

     TPlatforms=array[0..CountPlatforms-1] of TPlatform;

     TPlatformOrder=array[0..CountPlatforms-1] of integer;

     TPlayer=record
      OldX,OldY,x,y,DirectionX,Width,Height,JumpedHeight,JumpedHeightEx,JumpSpeed,FallSpeed,MaxJumpSpeed,MoveSpeed,CurrentMoveSpeed,Accelerometer:single;
      State,Direction,PlatformWithSpringIndex:integer;
      DeadTime,Score,Stars:int64;
      CollisionMap:PCollisionMap;
      IsSpringJump:boolean;
     end;

     PState=^TState;
     TState=record
      ReadyToRender:longint;
      DoInitTitleScreen:longint;
      DoInitGameEx:longint;
      DoInitGame:longint;
      DoFire:longint;
      CurrentTrack:longint;
      LastTrack:longint;
      PartNowTime:int64;
      FrameTime:int64;
      Clouds:TClouds;
      Fires:TFires;
      CircleOrder:TCircleOrder;
      Platforms:TPlatforms;
      PlatformOrder:TPlatformOrder;
      PlatformPhase:single;
      PlatformIndex:integer;
      Player:TPlayer;
      State:integer;
      CollisionPhase:boolean;
     end;

     TStates=array[0..$f] of TState;

     TRenderStateLocks=array[0..$f] of longint;

     TResolution=record
      Width:integer;
      Height:integer;
     end;

     TResolutions=array[0..0{6}] of TResolution;

var {TextureTitle,}TextureTitle2,TextureTitleScreen,TextureTitleScreenStart,TextureFont,
    TextureSolidPlatformAsleep,TextureSolidPlatformAwake,
    TextureImperviousPlatformNormal,TextureImperviousPlatformTouched,
    TextureSpring,TextureBG1,TextureFloorFront,TexturePrestartScreen,TexturePostgameScreen:TTexture;
    TextureClouds:array[0..2] of TTexture;
    HeightEx,HeightDiff,HeightEx2,HeightDiff2:integer;
    NowTime,LastTime,StartTime,PartStartTime:int64;
    State:TState;

    RenderStates:TStates;
    RenderStateLocks:TRenderStateLocks;

    SoundSystem:TBeRoSoundSystem;

    VectorCanvasFont:TBeRoVectorCanvasFont;
    VectorCanvas:TBeRoVectorCanvas;

    BestResolution:integer;
    BestResolutionRatioDiff:single;

    AnimationPlayerUp:TAnimation;
    AnimationPlayerDown:TAnimation;
    AnimationPlayerTransition:TAnimation;
    AnimationPlayerDie:TAnimation;

    AnimationMonsterWalk:TAnimation;
    AnimationMonsterDie:TAnimation;

    AnimationStar:TAnimation;

    AnimationFire:TAnimation;

    FontCharWidths:TFontCharWidths;

    StateIndex:longint;

    ReadStateIndex:longint;
    ReadStateIndexLocked:longint;

    DoUpload:boolean;

const FPSFilterFactor=0.99900048971; // = power(2.718281828,-(2.0*0.5)*0.001); // 0.5 Hz filter factor

      FPSex:single=0;
      FPS:integer=0;

      Resolutions:TResolutions=((Width:480;Height:800));
{
      Resolutions:TResolutions=((Width:200;Height:320),
                                (Width:320;Height:320),
                                (Width:480;Height:800),
                                (Width:480;Height:854),
                                (Width:600;Height:1024),
                                (Width:640;Height:960),
                                (Width:768;Height:1024));
}
{const Resolutions:TResolutions=((Width:200;Height:320),
                                (Width:320;Height:320),
                                (Width:320;Height:480),
                                (Width:480;Height:800),
                                (Width:480;Height:854),
                                (Width:480;Height:960),
                                (Width:600;Height:1024),
                                (Width:640;Height:960),
                                (Width:768;Height:1024));}

var Delta:single;

procedure ReinitPointers;
procedure InitClouds;
procedure InitFires;
procedure InitPlatforms;
procedure InitPlayer;
procedure InitTitleScreen;
procedure InitPrestartScreen;
procedure InitGame;
procedure InitPostgameScreen;
procedure Init;
procedure Done;
procedure CleanUp;
procedure Reinit(Width,Height:integer);
procedure Logic(DeltaTime:single);
function Render(DeltaInc:int64):boolean;
procedure Process(DeltaInc:int64);
procedure Suspend;
procedure Resume;
procedure UpdateSound;
procedure AccelerometerChange(x,y,z:single);
procedure Touch(x,y:single);
procedure BackToTitleScreen;

implementation

uses DataManager;

{$ifndef fpc}
{$ifdef cpu386}
function InterLockedDecrement(var Target:longint):longint; assembler; register;
asm
 mov edx,$ffffffff
 xchg eax,edx
 lock xadd [edx],eax
 dec eax
end;

function InterLockedIncrement(var Target:longint):longint; assembler; register;
asm
 mov edx,1
 xchg eax,edx
 lock xadd [edx],eax
 inc eax
end;

function InterLockedExchange(var Target:longint;Source:longint):longint; assembler; register;
asm
 xchg [eax],edx
 mov eax,edx
end;

function InterLockedExchangeAdd(var Target:longint;Source:longint):longint; assembler; register;
asm
 xchg edx,eax
 lock xadd [edx],eax
end;

function InterLockedCompareExchange(var Target:longint;NewValue,Comperand:longint):longint; assembler; register;
asm
 xchg ecx,eax
 lock cmpxchg [ecx],edx
end;

function InterLockedCompareExchange64(var Target:int64;NewValue,Comperand:int64):int64; assembler; stdcall;
asm
 push ebx
 push edi
 mov edi,dword ptr [Target]
 mov edx,dword ptr [Comperand+4]
 mov eax,dword ptr [Comperand+0]
 mov ecx,dword ptr [NewValue+4]
 mov ebx,dword ptr [NewValue+0]
 lock cmpxchg8b [edi]
 pop edi
 pop ebx
end;
{$endif}
{$else}
{function InterLockedDecrement(var Target:longint):longint;
begin
 result:=Target;
 dec(Target);
end;

function InterLockedIncrement(var Target:longint):longint;
begin
 result:=Target;
 inc(Target);
end;

function InterLockedExchange(var Target:longint;Source:longint):longint;
begin
 result:=Target;
 Target:=Source;
end;

function InterLockedExchangeAdd(var Target:longint;Source:longint):longint;
begin
 result:=Target;
 Target:=Source+1;
end;

function InterLockedCompareExchange(var Target:longint;NewValue,Comperand:longint):longint;
begin
 result:=Target;
 if result=Comperand then begin
  Target:=NewValue;
 end;
end;

function InterLockedCompareExchange64(var Target:int64;NewValue,Comperand:int64):int64;
begin
 result:=Target;
 if result=Comperand then begin
  Target:=NewValue;
 end;
end;{}
{$endif}

procedure ReinitPointers;
var i:integer;
begin
 for i:=0 to CountFires-1 do begin
  State.Fires[i].CollisionMap:=AnimationFire.GetCollisionMap(false);
 end;
 for i:=0 to CountPlatforms-1 do begin
  State.Platforms[i].CollisionMap:=TextureSolidPlatformAsleep.GetCollisionMap(false);
  State.Platforms[i].SpringCollisionMap:=TextureSpring.GetCollisionMap(false);
  State.Platforms[i].StarCollisionMap:=AnimationStar.GetCollisionMap(false);
  State.Platforms[i].MonsterCollisionMap:=AnimationMonsterWalk.GetCollisionMap(false);
 end;
 State.Player.CollisionMap:=AnimationPlayerUp.GetCollisionMap(false);
end;

procedure InitClouds;
var i:integer;
begin
 for i:=0 to CountClouds-1 do begin
  State.Clouds[i].y:=(((i-0.5)*2)/CountClouds)*CANVAS_HEIGHT;
  State.Clouds[i].k:=min(max(random(length(TextureClouds)),0),length(TextureClouds)-1);
  State.Clouds[i].h:=min((random*CANVAS_WIDTH*0.25)+(CANVAS_WIDTH*0.125),TextureClouds[State.Clouds[i].k].Height*MaxCloudScale);
  State.Clouds[i].w:=(TextureClouds[State.Clouds[i].k].Width*State.Clouds[i].h)/TextureClouds[State.Clouds[i].k].Height;
  State.Clouds[i].x:=(random*CANVAS_WIDTH)-(State.Clouds[i].w*0.5);
  State.Clouds[i].a:=(random*0.6)+0.2;
  State.Clouds[i].s:=(random*0.5)+0.75;
  State.CircleOrder[i]:=i;
 end;
end;

procedure InitFires;
var i:integer;
begin
 FillChar(State.Fires,sizeof(TFires),#0);
 for i:=0 to CountFires-1 do begin
  State.Fires[i].Active:=false;
  State.Fires[i].w:=AnimationFire.Width;
  State.Fires[i].h:=AnimationFire.Height;
  State.Fires[i].CollisionMap:=AnimationFire.GetCollisionMap(false);
 end;
end;

procedure InitPlatforms;
var i:integer;
    Phase:boolean;
begin
 State.PlatformPhase:=0;
 State.PlatformIndex:=0;
 case State.State of
  sTITLESCREEN:begin
   for i:=0 to CountPlatforms-1 do begin
    State.Platforms[i].Visible:=true;
    State.Platforms[i].Kind:=0;//random(256);
    State.Platforms[i].Width:=TextureSolidPlatformAsleep.Width;
    State.Platforms[i].Height:=TextureSolidPlatformAsleep.Height;
    State.Platforms[i].x:=((0.5+(cos(State.PlatformPhase)*0.5))*(CANVAS_WIDTH-(State.Platforms[i].Width*1.125)))+(State.Platforms[i].Width*0.0625);
    State.PlatformPhase:=State.PlatformPhase+(0.25*PI);
    State.Platforms[i].y:=(((CountPlatforms-(i+1))*(HeightEx/CountPlatforms))-HeightDiff)-50;
    State.Platforms[i].IsMoving:=false;//random(256)<$10;
  //  State.Platforms[i].IsMoving:=random(256)<$10;
    State.Platforms[i].Direction:=(random(2)-0.5)*2;
    State.Platforms[i].Touched:=false;
    State.Platforms[i].TouchedFromDown:=false;
    State.Platforms[i].SpringTouched:=false;
    State.Platforms[i].MonsterTouched:=false;
    State.Platforms[i].Spring:=false;
    State.Platforms[i].Monster:=false;
    State.Platforms[i].MonsterDead:=false;
    State.Platforms[i].MonsterDeadTime:=0;
    State.Platforms[i].CollisionMap:=TextureSolidPlatformAsleep.GetCollisionMap(false);
    State.Platforms[i].SpringX:=State.Platforms[i].x+((State.Platforms[i].Width-TextureSpring.Width)*0.5);
    State.Platforms[i].SpringY:=State.Platforms[i].y+SpringOfsY;
    State.Platforms[i].SpringYOfs:=0;
    State.Platforms[i].SpringYSpeed:=0;
    State.Platforms[i].SpringWidth:=TextureSpring.Width;
    State.Platforms[i].SpringHeight:=TextureSpring.Height;
    State.Platforms[i].SpringState:=ssNONE;
    State.Platforms[i].SpringCollisionMap:=TextureSpring.GetCollisionMap(false);
    State.Platforms[i].Star:=(random(256)<$40) and not State.Platforms[i].Spring;
    State.Platforms[i].StarX:=State.Platforms[i].x+((State.Platforms[i].Width-AnimationStar.Width)*0.5);
    State.Platforms[i].StarY:=(State.Platforms[i].y-AnimationStar.Height)+((random*10)-5);
    State.Platforms[i].StarWidth:=AnimationStar.Width;
    State.Platforms[i].StarHeight:=AnimationStar.Height;
    State.Platforms[i].StarCollisionMap:=AnimationStar.GetCollisionMap(false);
    State.Platforms[i].MonsterDirection:=1;
    State.Platforms[i].MonsterFallSpeed:=0;
    State.Platforms[i].MonsterXOfs:=0;
    State.Platforms[i].MonsterYOfs:=0;
    State.Platforms[i].MonsterX:=State.Platforms[i].x+((State.Platforms[i].Width-AnimationMonsterWalk.Width)*0.5);
    State.Platforms[i].MonsterX2:=State.Platforms[i].MonsterX;
    State.Platforms[i].MonsterY:=(State.Platforms[i].y+16)-AnimationMonsterWalk.Height;
    State.Platforms[i].MonsterWidth:=AnimationMonsterWalk.Width;
    State.Platforms[i].MonsterHeight:=AnimationMonsterWalk.Height;
    State.Platforms[i].MonsterCollisionMap:=AnimationMonsterWalk.GetCollisionMap(false);
   end;
  end;
  sGAME:begin
   for i:=0 to CountPlatforms-1 do begin
    State.Platforms[i].Visible:=true;
    State.Platforms[i].Kind:=random(256);
    State.Platforms[i].Width:=TextureSolidPlatformAsleep.Width;
    State.Platforms[i].Height:=TextureSolidPlatformAsleep.Height;
    State.Platforms[i].x:=((0.5+(cos(State.PlatformPhase)*0.5))*(CANVAS_WIDTH-(State.Platforms[i].Width*1.125)))+(State.Platforms[i].Width*0.0625);
    if State.Platforms[i].Kind=42 then begin
     State.PlatformPhase:=State.PlatformPhase+max(random*PI,0.5*PI);
    end else begin
     State.PlatformPhase:=State.PlatformPhase+(random*2*PI);
    end;
    State.Platforms[i].y:=(((CountPlatforms-(i+1))*(HeightEx/CountPlatforms))-HeightDiff)-50;
    State.Platforms[i].IsMoving:=random(256)<$10;
    State.Platforms[i].Direction:=(random(2)-0.5)*2;
    State.Platforms[i].Touched:=false;
    State.Platforms[i].TouchedFromDown:=false;
    State.Platforms[i].SpringTouched:=false;
    State.Platforms[i].MonsterTouched:=false;
    State.Platforms[i].Spring:=(i>(CountPlatforms shr 1)) and (random(256)<$10);
    State.Platforms[i].Monster:=(i>(CountPlatforms shr 1)) and (random(256)<$10);
    State.Platforms[i].MonsterDead:=false;
    State.Platforms[i].MonsterDeadTime:=0;
    State.Platforms[i].CollisionMap:=TextureSolidPlatformAsleep.GetCollisionMap(false);
    State.Platforms[i].SpringX:=State.Platforms[i].x+((State.Platforms[i].Width-TextureSpring.Width)*0.5);
    State.Platforms[i].SpringY:=State.Platforms[i].y+SpringOfsY;
    State.Platforms[i].SpringYOfs:=0;
    State.Platforms[i].SpringYSpeed:=0;
    State.Platforms[i].SpringWidth:=TextureSpring.Width;
    State.Platforms[i].SpringHeight:=TextureSpring.Height;
    State.Platforms[i].SpringState:=ssNONE;
    State.Platforms[i].SpringCollisionMap:=TextureSpring.GetCollisionMap(false);
    State.Platforms[i].Star:=(random(256)<$40) and not (State.Platforms[i].Spring or State.Platforms[i].Monster);
    State.Platforms[i].StarX:=State.Platforms[i].x+((State.Platforms[i].Width-AnimationStar.Width)*0.5);
    State.Platforms[i].StarY:=(State.Platforms[i].y-AnimationStar.Height)+((random*10)-5);
    State.Platforms[i].StarWidth:=AnimationStar.Width;
    State.Platforms[i].StarHeight:=AnimationStar.Height;
    State.Platforms[i].StarCollisionMap:=AnimationStar.GetCollisionMap(false);
    State.Platforms[i].MonsterDirection:=1;
    State.Platforms[i].MonsterFallSpeed:=0;
    State.Platforms[i].MonsterXOfs:=0;
    State.Platforms[i].MonsterYOfs:=0;
    State.Platforms[i].MonsterX:=State.Platforms[i].x+((State.Platforms[i].Width-AnimationMonsterWalk.Width)*0.5);
    State.Platforms[i].MonsterX2:=State.Platforms[i].MonsterX;
    State.Platforms[i].MonsterY:=(State.Platforms[i].y+16)-AnimationMonsterWalk.Height;
    State.Platforms[i].MonsterWidth:=AnimationMonsterWalk.Width;
    State.Platforms[i].MonsterHeight:=AnimationMonsterWalk.Height;
    State.Platforms[i].MonsterCollisionMap:=AnimationMonsterWalk.GetCollisionMap(false);
   end;
  end;
 end;
 for i:=0 to CountPlatforms-1 do begin
  for Phase:=low(State.Platforms[i].Collision) to high(State.Platforms[i].Collision) do begin
   State.Platforms[i].Collision[Phase]:=false;
  end;
  for Phase:=low(State.Platforms[i].SpringCollision) to high(State.Platforms[i].SpringCollision) do begin
   State.Platforms[i].SpringCollision[Phase]:=false;
  end;
  for Phase:=low(State.Platforms[i].MonsterCollision) to high(State.Platforms[i].MonsterCollision) do begin
   State.Platforms[i].MonsterCollision[Phase]:=false;
  end;
  for Phase:=low(State.Platforms[i].MonsterFireCollisionNo) to high(State.Platforms[i].MonsterFireCollisionNo) do begin
   State.Platforms[i].MonsterFireCollisionNo[Phase]:=0;
  end;
  for Phase:=low(State.Platforms[i].StarCollision) to high(State.Platforms[i].StarCollision) do begin
   State.Platforms[i].StarCollision[Phase]:=false;
  end;
  State.Platforms[i].InRange:=false;
  State.Platforms[i].SpringInRange:=false;
  State.Platforms[i].MonsterInRange:=false;
  State.Platforms[i].StarInRange:=false;
  State.PlatformOrder[i]:=i;
 end;
end;

procedure InitPlayer;
begin
 State.Player.Width:=AnimationPlayerUp.Width;
 State.Player.Height:=AnimationPlayerUp.Height;
 State.Player.x:=(CANVAS_WIDTH-State.Player.Width)*0.5;
 State.Player.y:=(CANVAS_HEIGHT-State.Player.Height)-(50-14);
 State.Player.JumpedHeight:=0;
 State.Player.JumpedHeightEx:=0;
 State.Player.JumpSpeed:=16;
 State.Player.MaxJumpSpeed:=16;
 State.Player.FallSpeed:=0;
 State.Player.MoveSpeed:=0;
 State.Player.CurrentMoveSpeed:=0;
 State.Player.Accelerometer:=0;
 State.Player.State:=psJUMPING;
 State.Player.Direction:=1;
 State.Player.DirectionX:=-262144;
 State.Player.DeadTime:=0;
 State.Player.IsSpringJump:=false;
 State.Player.Score:=0;
 State.Player.Stars:=0;
 State.Player.OldX:=State.Player.x;
 State.Player.OldY:=State.Player.y;
 State.Player.PlatformWithSpringIndex:=0;
 State.CollisionPhase:=false;
 AnimationPlayerUp.SetFrame(0);
 State.Player.CollisionMap:=AnimationPlayerUp.GetCollisionMap(false);
end;

procedure InitTitleScreen;
begin
 State.State:=sTITLESCREEN;
 Randomize;
 InitClouds;
 InitFires;
 InitPlatforms;
 InitPlayer;
 PartStartTime:=NowTime;
end;

procedure InitPrestartScreen;
begin
 State.State:=sPRESTARTSCREEN;
 Randomize;
 PartStartTime:=NowTime;
end;

procedure InitGame;
begin
 State.State:=sGAME;
 Randomize;
 InitClouds;
 InitFires;
 InitPlatforms;
 InitPlayer;
 PartStartTime:=NowTime;
end;

procedure InitPostgameScreen;
begin
 State.State:=sPOSTGAMESCREEN;
 Randomize;
 PartStartTime:=NowTime;
end;

procedure InitData;
var p:pointer;
    Size:longword;
    i:integer;
begin
 begin
  SoundSystem.SetMixerMusicVolume(0.5);
  SoundSystem.SetMixerSampleVolume(4.0);
  SoundSystem.SetMixerAGC(true);
 end;
 begin
  LogIt('Loading title screen music...');
  if DataManager.DataFileRead('music/titlescreen.xm',p,Size) then begin
   if SoundSystem.LoadXMTrack(0,p,Size,true,BSSXMTrackInterpolationCUBICSPLINE) then begin
    FreeMem(p);
    LogIt('Title screen music loaded...');
   end else begin
    FreeMem(p);
    LogIt('Can''t load title screen music!');
   end;
  end else begin
   LogIt('Can''t load title screen music!');
  end;
 end;
 begin
  LogIt('Loading sound effects..'); 
  if DataManager.DataFileRead('sounds/playerhit.wav',p,Size) then begin
   if not SoundSystem.LoadSample(10,p,Size,3) then begin
    LogIt('Can''t load a sound effect!');
   end;
   FreeMem(p);
  end else begin
   LogIt('Can''t load a sound effect!');
  end;
  if DataManager.DataFileRead('sounds/playerjump.wav',p,Size) then begin
   if not SoundSystem.LoadSample(20,p,Size,3) then begin
    LogIt('Can''t load a sound effect!');
   end;
   FreeMem(p);
  end else begin
   LogIt('Can''t load a sound effect!');
  end;
  if DataManager.DataFileRead('sounds/playerspringjump.wav',p,Size) then begin
   if not SoundSystem.LoadSample(30,p,Size,3) then begin
    LogIt('Can''t load a sound effect!');
   end;
   FreeMem(p);
  end else begin
   LogIt('Can''t load a sound effect!');
  end;
  if DataManager.DataFileRead('sounds/playerstarcollect.wav',p,Size) then begin
   if not SoundSystem.LoadSample(40,p,Size,3) then begin
    LogIt('Can''t load a sound effect!');
   end;
   FreeMem(p);
  end else begin
   LogIt('Can''t load a sound effect!');
  end;
  if DataManager.DataFileRead('sounds/playerfire.wav',p,Size) then begin
   if not SoundSystem.LoadSample(50,p,Size,3) then begin
    LogIt('Can''t load a sound effect!');
   end;
   FreeMem(p);
  end else begin
   LogIt('Can''t load a sound effect!');
  end;
  if DataManager.DataFileRead('sounds/playerdie.wav',p,Size) then begin
   if not SoundSystem.LoadSample(60,p,Size,3) then begin
    LogIt('Can''t load a sound effect!');
   end;
   FreeMem(p);
  end else begin
   LogIt('Can''t load a sound effect!');
  end;
  if DataManager.DataFileRead('sounds/firemonster.wav',p,Size) then begin
   if not SoundSystem.LoadSample(70,p,Size,3) then begin
    LogIt('Can''t load a sound effect!');
   end;
   FreeMem(p);
  end else begin
   LogIt('Can''t load a sound effect!');
  end;
 end;
 begin
  if DataManager.DataFileRead('fonts/vera.png',p,Size) then begin
   TextureFont:=CreateTextureFromPNG(p,Size,true,false,false);
   FreeMem(p);
  end else begin
   TextureFont:=TTexture.Create(nil,512,512,tfRGBA8888,true,false,false);
  end;
 end;
 begin
  if DataManager.DataFileRead('fonts/vera.dat',p,Size) then begin
   Move(p^,FontCharWidths,sizeof(TFontCharWidths));
   FreeMem(p);
  end else begin
   FillChar(FontCharWidths,sizeof(TFontCharWidths),$00);
  end;
 end;
 begin
  if DataManager.DataFileRead('graphics/platforms/solid/asleep.png',p,Size) then begin
   TextureSolidPlatformAsleep:=CreateTextureFromPNG(p,Size,true,false,true);
   FreeMem(p);
  end else begin
   TextureSolidPlatformAsleep:=TTexture.Create(nil,512,512,tfRGBA8888,true,false,true);
  end;
 end;
 begin
  if DataManager.DataFileRead('graphics/platforms/solid/awake.png',p,Size) then begin
   TextureSolidPlatformAwake:=CreateTextureFromPNG(p,Size,true,false,true);
   FreeMem(p);
  end else begin
   TextureSolidPlatformAwake:=TTexture.Create(nil,512,512,tfRGBA8888,true,false,true);
  end;
 end;
 begin
  if DataManager.DataFileRead('graphics/platforms/impervious/normal.png',p,Size) then begin
   TextureImperviousPlatformNormal:=CreateTextureFromPNG(p,Size,true,false,true);
   FreeMem(p);
  end else begin
   TextureImperviousPlatformNormal:=TTexture.Create(nil,512,512,tfRGBA8888,true,false,true);
  end;
 end;
 begin
  if DataManager.DataFileRead('graphics/platforms/impervious/touched.png',p,Size) then begin
   TextureImperviousPlatformTouched:=CreateTextureFromPNG(p,Size,true,false,true);
   FreeMem(p);
  end else begin
   TextureImperviousPlatformTouched:=TTexture.Create(nil,512,512,tfRGBA8888,true,false,true);
  end;
 end;
 begin
  if DataManager.DataFileRead('graphics/spring/1.png',p,Size) then begin
   TextureSpring:=CreateTextureFromPNG(p,Size,true,false,true);
   FreeMem(p);
  end else begin
   TextureSpring:=TTexture.Create(nil,512,512,tfRGBA8888,true,false,true);
  end;
 end;
 begin
  if DataManager.DataFileRead('graphics/backgrounds/1.png',p,Size) then begin
   TextureBG1:=CreateTextureFromPNG(p,Size,true,false,false);
   FreeMem(p);
  end else begin
   TextureBG1:=TTexture.Create(nil,512,512,tfRGBA8888,true,false,false);
  end;
 end;
 begin
  if DataManager.DataFileRead('graphics/floor/front/1.png',p,Size) then begin
   TextureFloorFront:=CreateTextureFromPNG(p,Size,true,false,false);
   FreeMem(p);
  end else begin
   TextureFloorFront:=TTexture.Create(nil,512,512,tfRGBA8888,true,false,false);
  end;
 end;
 begin
  if DataManager.DataFileRead('prestartscreen/prestartscreen.png',p,Size) then begin
   TexturePrestartScreen:=CreateTextureFromPNG(p,Size,true,false,false);
   FreeMem(p);
  end else begin
   TexturePrestartScreen:=TTexture.Create(nil,512,512,tfRGBA8888,true,false,false);
  end;
 end;
 begin
  if DataManager.DataFileRead('postgamescreen/postgamescreen.png',p,Size) then begin
   TexturePostgameScreen:=CreateTextureFromPNG(p,Size,true,false,false);
   FreeMem(p);
  end else begin
   TexturePostgameScreen:=TTexture.Create(nil,512,512,tfRGBA8888,true,false,false);
  end;
 end;
 begin
  if DataManager.DataFileRead('titlescreens/start.png',p,Size) then begin
   TextureTitleScreenStart:=CreateTextureFromPNG(p,Size,true,false,false);
   FreeMem(p);
  end else begin
   TextureTitleScreenStart:=TTexture.Create(nil,128,256,tfRGBA8888,true,false,false);
  end;
 end;
 begin
  for i:=low(TextureClouds) to high(TextureClouds) do begin
   if DataManager.DataFileRead('graphics/clouds/'+inttostr(i+1)+'.png',p,Size) then begin
    TextureClouds[i]:=CreateTextureFromPNG(p,Size,true,false,false);
    FreeMem(p);
   end else begin
    TextureClouds[i]:=TTexture.Create(nil,512,512,tfRGBA8888,true,false,false);
   end;
  end;
 end;
 begin
  AnimationPlayerUp:=TAnimation.Create;
  for i:=1 to 256 do begin
   if DataManager.DataFileRead('graphics/player/up/'+inttostr(i)+'.png',p,Size) then begin
    AnimationPlayerUp.Add(CreateTextureFromPNG(p,Size,true,false,true));
    FreeMem(p);
   end else begin
    break;
   end;
  end;
 end;
 begin
  AnimationPlayerDown:=TAnimation.Create;
  for i:=1 to 256 do begin
   if DataManager.DataFileRead('graphics/player/down/'+inttostr(i)+'.png',p,Size) then begin
    AnimationPlayerDown.Add(CreateTextureFromPNG(p,Size,true,false,true));
    FreeMem(p);
   end else begin
    break;
   end;
  end;
 end;
 begin
  AnimationPlayerTransition:=TAnimation.Create;
  for i:=1 to 256 do begin
   if DataManager.DataFileRead('graphics/player/transition/'+inttostr(i)+'.png',p,Size) then begin
    AnimationPlayerTransition.Add(CreateTextureFromPNG(p,Size,true,false,true));
    FreeMem(p);
   end else begin
    break;
   end;
  end;
 end;
 begin
  AnimationPlayerDie:=TAnimation.Create;
  for i:=1 to 256 do begin
   if DataManager.DataFileRead('graphics/player/die/'+inttostr(i)+'.png',p,Size) then begin
    AnimationPlayerDie.Add(CreateTextureFromPNG(p,Size,true,false,true));
    FreeMem(p);
   end else begin
    break;
   end;
  end;
 end;
 begin
  AnimationMonsterWalk:=TAnimation.Create;
  for i:=1 to 256 do begin
   if DataManager.DataFileRead('graphics/monster/walk/'+inttostr(i)+'.png',p,Size) then begin
    AnimationMonsterWalk.Add(CreateTextureFromPNG(p,Size,true,false,true));
    FreeMem(p);
   end else begin
    break;
   end;
  end;
 end;
 begin
  AnimationMonsterDie:=TAnimation.Create;
  for i:=1 to 256 do begin
   if DataManager.DataFileRead('graphics/monster/die/'+inttostr(i)+'.png',p,Size) then begin
    AnimationMonsterDie.Add(CreateTextureFromPNG(p,Size,true,false,true));
    FreeMem(p);
   end else begin
    break;
   end;
  end;
 end;
 begin
  AnimationStar:=TAnimation.Create;
  for i:=1 to 256 do begin
   if DataManager.DataFileRead('graphics/star/'+inttostr(i)+'.png',p,Size) then begin
    AnimationStar.Add(CreateTextureFromPNG(p,Size,true,false,true));
    FreeMem(p);
   end else begin
    break;
   end;
  end;
 end;
 begin
  AnimationFire:=TAnimation.Create;
  for i:=1 to 256 do begin
   if DataManager.DataFileRead('graphics/star/'+inttostr(i)+'.png',p,Size) then begin
    AnimationFire.Add(CreateTextureFromPNG(p,Size,true,false,true));
    FreeMem(p);
   end else begin
    break;
   end;
  end;
 end;
end;

procedure Init;
var p:pointer;
  //  x,y,i,v:integer;
    //vf:single;
    Size:longword;
begin
 LastBestResolution:=-1;
 try
  FirstFrameWasDrawn:=false;
  Randomize;
  StateIndex:=0;
  ReadStateIndex:=0;
  ReadStateIndexLocked:=0;
  FillChar(State,sizeof(TState),#0);
  FillChar(RenderStates,sizeof(TStates),#0);
  FillChaR(RenderStateLocks,sizeof(TRenderStateLocks),#0);
  State.ReadyToRender:=0;
  State.CurrentTrack:=-1;
  State.LastTrack:=-1;
  begin
   InitData;
   begin
    VectorCanvas:=TBeRoVectorCanvas.Create;
    VectorCanvas.Setup(512,512,bvcrmCOLORINTENSITY);
    VectorCanvas.LineCapMode:=bvclcmSQUARE;
    VectorCanvas.LineJoinMode:=bvcljmMITER;
    VectorCanvas.LineInnerJoinMode:=bvclijmMITER;
    VectorCanvas.StyleMode:=bvcsmFILL;
    VectorCanvas.Winding:=true;
    VectorCanvas.CustomColorProc:=nil;
    VectorCanvas.HandleUDD:=false;
   end;
   begin
    VectorCanvasFont:=TBeRoVectorCanvasFont.Create;
    if DataManager.DataFileRead('fonts/vera.ttf',p,Size) then begin
     VectorCanvasFont.LoadTrueType(p^,Size);
     FreeMem(p);
    end;
   end;
   begin
    HeightDiff2:=64+8;
    HeightDiff:=HeightDiff2*2;
    HeightEx:=CANVAS_HEIGHT+HeightDiff;
    HeightEx2:=CANVAS_HEIGHT+HeightDiff2;
   end;
(*  begin
    GetMem(p,512*512*4);
    FillChar(p^,512*512*4,#$00);

    VectorCanvas.LineCapMode:=bvclcmSQUARE;
    VectorCanvas.LineJoinMode:=bvcljmMITER;
    VectorCanvas.LineInnerJoinMode:=bvclijmMITER;
    VectorCanvas.StyleMode:=bvcsmFILL;
    VectorCanvas.Winding:=true;
    VectorCanvas.CustomColorProc:=nil;
    VectorCanvas.HandleUDD:=false;

    VectorCanvas.Color:=$ff88ddee;
    VectorCanvasFont.Size:=-64;
    Move(p^,VectorCanvas.CurrentCanvas^,512*512*4);

    VectorCanvasFont.Styles:=[bvcfsBOLD];
    VectorCanvas.Color:=$ff8888ff;
    VectorCanvasFont.Size:=-24;
    VectorCanvasFont.Draw(VectorCanvas,(256*256)-(VectorCanvasFont.TextWidth('A game by') shr 1),((450+(VectorCanvasFont.Size div 2))*256),'A game by',0,false);
    VectorCanvasFont.Draw(VectorCanvas,(256*256)-(VectorCanvasFont.TextWidth('Benjamin Rosseaux and Alex Brem') shr 1),((450-(VectorCanvasFont.Size div 2))*256),'Benjamin Rosseaux and Alex Brem',0,false);
    VectorCanvasFont.Draw(VectorCanvas,(256*256)-(VectorCanvasFont.TextWidth('featuring ''foembs'' by Christoph Mütze') shr 1),(485*256),'featuring ''foembs'' by Christoph Mütze',0,false);
    VectorCanvas.Draw;

    VectorCanvasFont.Styles:=[bvcfsBOLD];
    VectorCanvas.Color:=$ff000000;
    VectorCanvasFont.Size:=-12;
    VectorCanvasFont.Draw(VectorCanvas,(256*256)-(VectorCanvasFont.TextWidth('Copyright (C) 2010, Benjamin Rosseaux, Alex Brem & Christoph Mütze') shr 1),((32+(VectorCanvasFont.Size*2))*256),'Copyright (C) 2010, Benjamin Rosseaux, Alex Brem & Christoph Mütze',0,false);
    VectorCanvas.Draw;

    VectorCanvas.StyleMode:=bvcsmLINE;
    VectorCanvas.LineWidth:=1*256;
    VectorCanvas.Winding:=true;
    VectorCanvas.CustomColorProc:=nil;
    VectorCanvas.HandleUDD:=false;

    VectorCanvas.Color:=$ff000000;
    VectorCanvasFont.Size:=-64;

    VectorCanvas.LineWidth:=1*128;
    VectorCanvasFont.Styles:=[bvcfsBOLD];
    VectorCanvasFont.Size:=-24;
    VectorCanvasFont.Draw(VectorCanvas,(256*256)-(VectorCanvasFont.TextWidth('A game by') shr 1),((450+(VectorCanvasFont.Size div 2))*256),'A game by',0,false);
    VectorCanvasFont.Draw(VectorCanvas,(256*256)-(VectorCanvasFont.TextWidth('Benjamin Rosseaux and Alex Brem') shr 1),((450-(VectorCanvasFont.Size div 2))*256),'Benjamin Rosseaux and Alex Brem',0,false);
    VectorCanvasFont.Draw(VectorCanvas,(256*256)-(VectorCanvasFont.TextWidth('featuring ''foembs'' by Christoph Mütze') shr 1),(485*256),'featuring ''foembs'' by Christoph Mütze',0,false);
    VectorCanvas.Draw;

  { VectorCanvas.LineWidth:=1*64;
    VectorCanvas.Color:=$ffffffff;
    VectorCanvasFont.Styles:=[bvcfsBOLD];
    VectorCanvasFont.Size:=-12;
    VectorCanvasFont.Draw(VectorCanvas,(256*256)-(VectorCanvasFont.TextWidth('Copyright (C) 2010, Benjamin Rosseaux, Alex Brem & Chistoph Mütze') shr 1),((32+(VectorCanvasFont.Size*2))*256),'Copyright (C) 2010, Benjamin Rosseaux, Alex Brem & Chistoph Mütze',0,false);
    VectorCanvas.Draw;}

    Move(VectorCanvas.CurrentCanvas^,p^,512*512*4);
    TextureTitle:=TTexture.Create(p,512,512,tfRGBA8888,true,false,false);
   end;*)
   begin
    VectorCanvas.Setup(256,64,bvcrmCOLORINTENSITY);
    GetMem(p,256*64*4);
    FillChar(p^,256*64*4,#$00);

    VectorCanvas.LineCapMode:=bvclcmSQUARE;
    VectorCanvas.LineJoinMode:=bvcljmMITER;
    VectorCanvas.LineInnerJoinMode:=bvclijmMITER;
    VectorCanvas.StyleMode:=bvcsmFILL;
    VectorCanvas.Winding:=true;
    VectorCanvas.CustomColorProc:=nil;
    VectorCanvas.HandleUDD:=false;

    VectorCanvas.Color:=$ff88ddee;
    VectorCanvasFont.Size:=-64;
    Move(p^,VectorCanvas.CurrentCanvas^,256*64*4);

    VectorCanvasFont.Styles:=[bvcfsBOLD];
    VectorCanvas.Color:=$ffeeddaa;
    VectorCanvasFont.Size:=-30;
    VectorCanvasFont.Draw(VectorCanvas,(128*256)-(VectorCanvasFont.TextWidth('TOUCH TO START') shr 1),(256*32)-(VectorCanvasFont.TextHeight('TOUCH TO START') shr 1),'TOUCH TO START',0,false);
    VectorCanvas.Draw;

    VectorCanvas.StyleMode:=bvcsmLINE;
    VectorCanvas.LineWidth:=1*256;
    VectorCanvas.Winding:=true;
    VectorCanvas.CustomColorProc:=nil;
    VectorCanvas.HandleUDD:=false;

    VectorCanvas.Color:=$ff000000;
    VectorCanvasFont.Size:=-64;

    VectorCanvasFont.Styles:=[bvcfsBOLD];
    VectorCanvasFont.Size:=-30;
    VectorCanvasFont.Draw(VectorCanvas,(128*256)-(VectorCanvasFont.TextWidth('TOUCH TO START') shr 1),(256*32)-(VectorCanvasFont.TextHeight('TOUCH TO START') shr 1),'TOUCH TO START',0,false);
    VectorCanvas.Draw;

    Move(VectorCanvas.CurrentCanvas^,p^,256*64*4);
    TextureTitle2:=TTexture.Create(p,256,64,tfRGBA8888,true,false,false);
   end;
   begin
    TextureTitleScreen:=TTexture.Create(nil,512,512,tfRGBA8888,true,false,false);
   end;
   InitTitleScreen;
   Delta:=0;
   State.CurrentTrack:=0;
   SoundRunning:=false;
   DoUpload:=true;
  end;
 except
  on e:Exception do begin
   LogIt(e.Message);
  end;
 end;
end;

procedure Done;
var i:integer;
begin
 try
//FreeAndNil(TextureTitle);
  FreeAndNil(TextureTitle2);
  FreeAndNil(TextureTitleScreen);
  FreeAndNil(TextureFont);
  FreeAndNil(TextureSolidPlatformAsleep);
  FreeAndNil(TextureSolidPlatformAwake);
  FreeAndNil(TextureImperviousPlatformNormal);
  FreeAndNil(TextureImperviousPlatformTouched);
  FreeAndNil(TextureSpring);
  FreeAndNil(TextureBG1);
  FreeAndNil(TextureFloorFront);
  FreeAndNil(TexturePrestartScreen);
  FreeAndNil(TexturePostgameScreen);
  FreeAndNil(TextureTitleScreenStart);
  for i:=low(TextureClouds) to high(TextureClouds) do begin
   FreeAndNil(TextureClouds[i]);
  end;
  FreeAndNil(VectorCanvas);
  FreeAndNil(VectorCanvasFont);
  FreeAndNil(AnimationPlayerUp);
  FreeAndNil(AnimationPlayerDown);
  FreeAndNil(AnimationPlayerTransition);
  FreeAndNil(AnimationPlayerDie);
  FreeAndNil(AnimationMonsterWalk);
  FreeAndNil(AnimationMonsterDie);
  FreeAndNil(AnimationStar);
  FreeAndNil(AnimationFire);
  UnloadTextureShader;
  UnloadSolidShader;
  UnloadQuadVBO;
 except
  on e:Exception do begin
   LogIt(e.Message);
  end;
 end;
end;

procedure CleanUp;
var i:integer;
begin
 try
//TextureTitle.Unload;
  TextureTitle2.Unload;
  TextureTitleScreen.Unload;
  TextureFont.Unload;
  TextureSolidPlatformAsleep.Unload;
  TextureSolidPlatformAwake.Unload;
  TextureImperviousPlatformNormal.Unload;
  TextureImperviousPlatformTouched.Unload;
  TextureSpring.Unload;
  TextureBG1.Unload;
  TextureFloorFront.Unload;
  TexturePrestartScreen.Unload;
  TexturePostgameScreen.Unload;
  TextureTitleScreenStart.Unload;
  for i:=low(TextureClouds) to high(TextureClouds) do begin
   TextureClouds[i].Unload;
  end;
  AnimationPlayerUp.Unload;
  AnimationPlayerDown.Unload;
  AnimationPlayerTransition.Unload;
  AnimationPlayerDie.Unload;
  AnimationMonsterWalk.Unload;
  AnimationMonsterDie.Unload;
  AnimationStar.Unload;
  AnimationFire.Unload;
  UnloadTextureShader;
  UnloadSolidShader;
  UnloadQuadVBO;
  DoUpload:=false;
 except
  on e:Exception do begin
   LogIt(e.Message);
  end;
 end;
end;

procedure ProcessUpload;
var i:integer;
begin
 try
//TextureTitle.Upload;
  TextureTitle2.Upload;
  TextureTitleScreen.Upload;
  TextureFont.Upload;
  TextureSolidPlatformAsleep.Upload;
  TextureSolidPlatformAwake.Upload;
  TextureImperviousPlatformNormal.Upload;
  TextureImperviousPlatformTouched.Upload;
  TextureSpring.Upload;
  TextureBG1.Upload;
  TextureFloorFront.Upload;
  TexturePrestartScreen.Upload;
  TexturePostgameScreen.Upload;
  TextureTitleScreenStart.Upload;
  for i:=low(TextureClouds) to high(TextureClouds) do begin
   TextureClouds[i].Upload;
  end;
  AnimationPlayerUp.Upload;
  AnimationPlayerDown.Upload;
  AnimationPlayerTransition.Upload;
  AnimationPlayerDie.Upload;
  AnimationMonsterWalk.Upload;
  AnimationMonsterDie.Upload;
  AnimationStar.Upload;
  AnimationFire.Upload;
  DoUpload:=false;
 except
  on e:Exception do begin
   LogIt(e.Message);
  end;
 end;
end;

procedure Reinit(Width,Height:integer);
var i:integer;
    Ratio,RatioDiff:single;
    p:pointer;
    Size:longword;
begin
 try
  FirstFrameWasDrawn:=false;
  begin
   LogIt('Setting screen width and height...');
   SCREEN_WIDTH:=Width;
   SCREEN_HEIGHT:=Height;
  end;
  begin
   LogIt('Finding best resolution...');
   BestResolution:=0;
   BestResolutionRatioDiff:=1e10;
   Ratio:=SCREEN_WIDTH/SCREEN_HEIGHT;
   for i:=0 to length(Resolutions)-1 do begin
    RatioDiff:=abs((Resolutions[i].Width/Resolutions[i].Height)-Ratio);
    if RatioDiff<BestResolutionRatioDiff then begin
     BestResolution:=i;
     BestResolutionRatioDiff:=RatioDiff;
    end;
   end;
  end;
  begin
   if BestResolution<>LastBestResolution then begin
    LogIt('Loading title screen texture for the best found resolution...');
    LastBestResolution:=BestResolution;
    if assigned(TextureTitleScreen) then begin
     FreeAndNil(TextureTitleScreen);
    end;
    if DataManager.DataFileRead('titlescreens/'+inttostr(Resolutions[BestResolution].Width)+'x'+inttostr(Resolutions[BestResolution].Height)+'.png',p,Size) then begin
     TextureTitleScreen:=CreateTextureFromPNG(p,Size,true,false,false);
     FreeMem(p);
    end else begin
     TextureTitleScreen:=TTexture.Create(nil,512,512,tfRGBA8888,true,false,false);
    end;
   end;
  end;
  begin
   LogIt('Unloading texture shader...');
   UnloadTextureShader;
   LogIt('Unloading solid shader...');
   UnloadSolidShader;
   LogIt('Unloading quad VBO buffers...');
   UnloadQuadVBO;
   LogIt('Compiling texture shader...');
   CompileTextureShader;
   LogIt('Compiling solid shader...');
   CompileSolidShader;
   LogIt('Loading quad VBO buffers...');
   CompileQuadVBO;
  end;
  begin
   LogIt('Setting textures dirty...');
// TextureTitle.Dirty:=true;
   TextureTitle2.Dirty:=true;
   TextureTitleScreen.Dirty:=true;
   TextureFont.Dirty:=true;
   TextureSolidPlatformAsleep.Dirty:=true;
   TextureSolidPlatformAwake.Dirty:=true;
   TextureImperviousPlatformNormal.Dirty:=true;
   TextureImperviousPlatformTouched.Dirty:=true;
   TextureSpring.Dirty:=true;
   TextureBG1.Dirty:=true;
   TextureFloorFront.Dirty:=true;
   TexturePrestartScreen.Dirty:=true;
   TexturePostgameScreen.Dirty:=true;
   TextureTitleScreenStart.Dirty:=true;
   for i:=low(TextureClouds) to high(TextureClouds) do begin
    TextureClouds[i].Dirty:=true;
   end;
   AnimationPlayerUp.SetDirty;
   AnimationPlayerDown.SetDirty;
   AnimationPlayerTransition.SetDirty;
   AnimationPlayerDie.SetDirty;
   AnimationMonsterWalk.SetDirty;
   AnimationMonsterDie.SetDirty;
   AnimationStar.SetDirty;
   AnimationFire.SetDirty;
  end;
  LogIt('Reset delta  ...');
  Delta:=0;
  DoUpload:=true;
 except
  on e:Exception do begin
   LogIt(e.Message);
  end;
 end;
end;

procedure MoveClouds(y:single);
var i:integer;
begin
 for i:=0 to CountClouds-1 do begin
  State.Clouds[i].y:=State.Clouds[i].y+(y*State.Clouds[i].s);
  if (State.Clouds[i].y-State.Clouds[i].h)>=CANVAS_HEIGHT then begin
   State.Clouds[i].k:=min(max(random(length(TextureClouds)),0),length(TextureClouds)-1);
   State.Clouds[i].h:=min((random*CANVAS_WIDTH*0.25)+(CANVAS_WIDTH*0.125),TextureClouds[State.Clouds[i].k].Height*MaxCloudScale);
   State.Clouds[i].w:=(TextureClouds[State.Clouds[i].k].Width*State.Clouds[i].h)/TextureClouds[State.Clouds[i].k].Height;
   State.Clouds[i].x:=(random*CANVAS_WIDTH)-(State.Clouds[i].w*0.5);
   State.Clouds[i].y:=-((State.Clouds[i].h*2)+(random*CANVAS_WIDTH*0.375));
   State.Clouds[i].a:=(random*0.6)+0.2;
   State.Clouds[i].s:=(random*0.5)+0.75;
  end;
 end;
end;

procedure MoveFiresY(y:single);
var i:integer;
begin
 for i:=0 to CountFires-1 do begin
  if State.Fires[i].Active then begin
   State.Fires[i].y:=State.Fires[i].y+y;
   if ((State.Fires[i].y+State.Fires[i].h)<0) or ((State.Fires[i].y-State.Fires[i].h)>=CANVAS_HEIGHT) then begin
    State.Fires[i].Active:=false;
   end;
  end;
 end;
end;

procedure MoveFires(DeltaInc:single);
var i:integer;
begin
 for i:=0 to CountFires-1 do begin
  if State.Fires[i].Active then begin
   State.Fires[i].o:=State.Fires[i].o+DeltaInc;
   State.Fires[i].x:=State.Fires[i].x+(State.Fires[i].dx*DeltaInc);
   State.Fires[i].y:=State.Fires[i].y+(State.Fires[i].dy*DeltaInc);
   if (((State.Fires[i].x+State.Fires[i].w)<0) or (State.Fires[i].x>=CANVAS_WIDTH)) or
      (((State.Fires[i].y+State.Fires[i].h)<0) or (State.Fires[i].y>=CANVAS_HEIGHT)) then begin
    State.Fires[i].Active:=false;
   end;
  end;
 end;
end;

procedure SortPlatforms;
var i,j:integer;
begin
 i:=0;
 while i<(CountPlatforms-1) do begin
  if State.Platforms[State.PlatformOrder[i]].y>State.Platforms[State.PlatformOrder[i+1]].y then begin
   j:=State.PlatformOrder[i];
   State.PlatformOrder[i]:=State.PlatformOrder[i+1];
   State.PlatformOrder[i+1]:=j;
   if i>0 then begin
    dec(i);
   end else begin
    inc(i);
   end;
  end else begin
   inc(i);
  end;
 end;
end;                          

procedure MovePlatforms(y:single);
var i,j:integer;
    IsSpring:boolean;
begin
 case State.State of
  sTITLESCREEN:begin
   for i:=0 to CountPlatforms-1 do begin
    State.Platforms[i].y:=State.Platforms[i].y+y;
    State.Platforms[i].SpringY:=State.Platforms[i].SpringY+y;
    State.Platforms[i].StarY:=State.Platforms[i].StarY+y;
    State.Platforms[i].MonsterY:=State.Platforms[i].MonsterY+y;
    State.Platforms[i].Visible:=((State.Platforms[i].y+PlatformTopOfsY)<CANVAS_HEIGHT) or
                                (State.Platforms[i].Monster and (((State.Platforms[i].MonsterY+State.Platforms[i].MonsterYOfs+MonsterYOfs)+MonsterTopOfsY)<CANVAS_HEIGHT)) or
                                (State.Platforms[i].Spring and (((State.Platforms[i].SpringY+State.Platforms[i].SpringYOfs)+SpringTopOfsY)<CANVAS_HEIGHT)) or
                                (State.Platforms[i].Star and (State.Platforms[i].StarY<CANVAS_HEIGHT));
    if State.Platforms[i].y>=HeightEx2 then begin
     State.Platforms[i].Visible:=true;
     State.Platforms[i].Kind:=0;//random(256);
     State.Platforms[i].x:=((0.5+(cos(State.PlatformPhase)*0.5))*(CANVAS_WIDTH-(State.Platforms[i].Width*1.125)))+(State.Platforms[i].Width*0.0625);
     State.PlatformPhase:=State.PlatformPhase+(0.25*PI);
     State.Platforms[i].y:=State.Platforms[i].y-HeightEx;
     State.Platforms[i].IsMoving:=false;//random(256)<$10;
     State.Platforms[i].Direction:=(random(2)-0.5)*2;
     State.Platforms[i].Touched:=false;
     State.Platforms[i].TouchedFromDown:=false;
     State.Platforms[i].SpringTouched:=false;
     State.Platforms[i].MonsterTouched:=false;
     State.Platforms[i].Spring:=false;
     State.Platforms[i].Monster:=false;
     State.Platforms[i].MonsterDead:=false;
     State.Platforms[i].MonsterDeadTime:=0;
     State.Platforms[i].CollisionMap:=TextureSolidPlatformAsleep.GetCollisionMap(false);
     State.Platforms[i].SpringX:=State.Platforms[i].x+((State.Platforms[i].Width-TextureSpring.Width)*0.5);
     State.Platforms[i].SpringY:=State.Platforms[i].y+SpringOfsY;
     State.Platforms[i].SpringYOfs:=0;
     State.Platforms[i].SpringYSpeed:=0;
     State.Platforms[i].SpringWidth:=TextureSpring.Width;
     State.Platforms[i].SpringHeight:=TextureSpring.Height;
     State.Platforms[i].SpringState:=ssNONE;
     State.Platforms[i].SpringCollisionMap:=TextureSpring.GetCollisionMap(false);
     State.Platforms[i].Star:=(random(256)<$40) and not (State.Platforms[i].Spring or State.Platforms[i].Monster);
     State.Platforms[i].StarX:=State.Platforms[i].x+((State.Platforms[i].Width-AnimationStar.Width)*0.5);
     State.Platforms[i].StarY:=(State.Platforms[i].y-AnimationStar.Height)+((random*10)-5);
     State.Platforms[i].StarWidth:=AnimationStar.Width;
     State.Platforms[i].StarHeight:=AnimationStar.Height;
     State.Platforms[i].StarCollisionMap:=AnimationStar.GetCollisionMap(false);
     State.Platforms[i].MonsterDirection:=1;
     State.Platforms[i].MonsterFallSpeed:=0;
     State.Platforms[i].MonsterXOfs:=0;
     State.Platforms[i].MonsterYOfs:=0;
     State.Platforms[i].MonsterX:=State.Platforms[i].x+((State.Platforms[i].Width-AnimationMonsterWalk.Width)*0.5);
     State.Platforms[i].MonsterX2:=State.Platforms[i].MonsterX;
     State.Platforms[i].MonsterY:=(State.Platforms[i].y+16)-AnimationMonsterWalk.Height;
     State.Platforms[i].MonsterWidth:=AnimationMonsterWalk.Width;
     State.Platforms[i].MonsterHeight:=AnimationMonsterWalk.Height;
     State.Platforms[i].MonsterCollisionMap:=AnimationMonsterWalk.GetCollisionMap(false);
    end;
   end;
  end;
  sGAME:begin
   for i:=0 to CountPlatforms-1 do begin
    State.Platforms[i].y:=State.Platforms[i].y+y;
    State.Platforms[i].SpringY:=State.Platforms[i].SpringY+y;
    State.Platforms[i].StarY:=State.Platforms[i].StarY+y;
    State.Platforms[i].MonsterY:=State.Platforms[i].MonsterY+y;
    State.Platforms[i].Visible:=((State.Platforms[i].y+PlatformTopOfsY)<CANVAS_HEIGHT) or
                                (State.Platforms[i].Monster and (((State.Platforms[i].MonsterY+State.Platforms[i].MonsterYOfs+MonsterYOfs)+MonsterTopOfsY)<CANVAS_HEIGHT)) or
                                (State.Platforms[i].Spring and (((State.Platforms[i].SpringY+State.Platforms[i].SpringYOfs)+SpringTopOfsY)<CANVAS_HEIGHT)) or
                                (State.Platforms[i].Star and (State.Platforms[i].StarY<CANVAS_HEIGHT));
    if State.Platforms[i].y>=HeightEx2 then begin
     State.Platforms[i].Visible:=true;
     State.Platforms[i].Kind:=random(256);
     State.Platforms[i].x:=((0.5+(cos(State.PlatformPhase)*0.5))*(CANVAS_WIDTH-(State.Platforms[i].Width*1.125)))+(State.Platforms[i].Width*0.0625);
     State.Platforms[i].y:=State.Platforms[i].y-HeightEx;
     IsSpring:=State.Player.IsSpringJump;
     for j:=0 to CountPlatforms-1 do begin
      if (State.Platforms[j].y>State.Platforms[i].y) and State.Platforms[j].Spring then begin
       IsSpring:=true;
       break;
      end;
     end;
     if IsSpring and (State.Platforms[i].Kind=42) then begin
      State.Platforms[i].Kind:=0;
     end;
     if State.Platforms[i].Kind=42 then begin
      State.PlatformPhase:=State.PlatformPhase+max(random*PI,0.5*PI);
     end else begin
      State.PlatformPhase:=State.PlatformPhase+(random*2*PI);
     end;
     State.Platforms[i].IsMoving:=random(256)<$10;
     State.Platforms[i].Direction:=(random(2)-0.5)*2;
     State.Platforms[i].Touched:=false;
     State.Platforms[i].TouchedFromDown:=false;
     State.Platforms[i].SpringTouched:=false;
     State.Platforms[i].MonsterTouched:=false;
     State.Platforms[i].Spring:=random(256)<$10;
     State.Platforms[i].Monster:=(random(256)<$10) and not IsSpring;
     State.Platforms[i].MonsterDead:=false;
     State.Platforms[i].MonsterDeadTime:=0;
     State.Platforms[i].CollisionMap:=TextureSolidPlatformAsleep.GetCollisionMap(false);
     State.Platforms[i].SpringX:=State.Platforms[i].x+((State.Platforms[i].Width-TextureSpring.Width)*0.5);
     State.Platforms[i].SpringY:=State.Platforms[i].y+SpringOfsY;
     State.Platforms[i].SpringYOfs:=0;
     State.Platforms[i].SpringYSpeed:=0;
     State.Platforms[i].SpringWidth:=TextureSpring.Width;
     State.Platforms[i].SpringHeight:=TextureSpring.Height;
     State.Platforms[i].SpringState:=ssNONE;
     State.Platforms[i].SpringCollisionMap:=TextureSpring.GetCollisionMap(false);
     State.Platforms[i].Star:=(random(256)<$40) and not (State.Platforms[i].Spring or State.Platforms[i].Monster);
     State.Platforms[i].StarX:=State.Platforms[i].x+((State.Platforms[i].Width-AnimationStar.Width)*0.5);
     State.Platforms[i].StarY:=(State.Platforms[i].y-AnimationStar.Height)+((random*10)-5);
     State.Platforms[i].StarWidth:=AnimationStar.Width;
     State.Platforms[i].StarHeight:=AnimationStar.Height;
     State.Platforms[i].StarCollisionMap:=AnimationStar.GetCollisionMap(false);
     State.Platforms[i].MonsterDirection:=1;
     State.Platforms[i].MonsterFallSpeed:=0;
     State.Platforms[i].MonsterXOfs:=0;
     State.Platforms[i].MonsterYOfs:=0;
     State.Platforms[i].MonsterX:=State.Platforms[i].x+((State.Platforms[i].Width-AnimationMonsterWalk.Width)*0.5);
     State.Platforms[i].MonsterX2:=State.Platforms[i].MonsterX;
     State.Platforms[i].MonsterY:=(State.Platforms[i].y+16)-AnimationMonsterWalk.Height;
     State.Platforms[i].MonsterWidth:=AnimationMonsterWalk.Width;
     State.Platforms[i].MonsterHeight:=AnimationMonsterWalk.Height;
     State.Platforms[i].MonsterCollisionMap:=AnimationMonsterWalk.GetCollisionMap(false);
    end;
   end;
  end;
 end;
end;

procedure MoveMovingPlatforms(Delta:single);
var i:integer;
begin
 for i:=0 to CountPlatforms-1 do begin
  if State.Platforms[i].IsMoving then begin
   State.Platforms[i].x:=State.Platforms[i].x+(State.Platforms[i].Direction*Delta);
   if State.Platforms[i].x<4 then begin
    State.Platforms[i].x:=4;
    State.Platforms[i].Direction:=-State.Platforms[i].Direction;
   end else if State.Platforms[i].x>((CANVAS_WIDTH-State.Platforms[i].Width)-4) then begin
    State.Platforms[i].x:=(CANVAS_WIDTH-State.Platforms[i].Width)-4;
    State.Platforms[i].Direction:=-State.Platforms[i].Direction;
   end;
   State.Platforms[i].SpringX:=State.Platforms[i].x+((State.Platforms[i].Width-TextureSpring.Width)*0.5);
   State.Platforms[i].StarX:=State.Platforms[i].x+((State.Platforms[i].Width-AnimationStar.Width)*0.5);
   State.Platforms[i].MonsterX:=State.Platforms[i].x+((State.Platforms[i].Width-AnimationMonsterWalk.Width)*0.5);
  end;
  //(2.0-(abs(frac(State.PartNowTime*0.1)-0.5)*4.0))*15.0;
 end;
end;

procedure MoveMonsters(Delta:single);
var i:integer;
begin
 for i:=0 to CountPlatforms-1 do begin
  if State.Platforms[i].Monster then begin
   if State.Platforms[i].MonsterDead then begin
    if State.Player.State<>psDEAD then begin
     State.Platforms[i].MonsterDeadTime:=State.Platforms[i].MonsterDeadTime+(Delta*0.0625);
     if State.Platforms[i].MonsterDeadTime>0.285714286 then begin
      State.Platforms[i].MonsterYOfs:=State.Platforms[i].MonsterYOfs+(State.Platforms[i].MonsterFallSpeed*Delta);
      State.Platforms[i].MonsterFallSpeed:=State.Platforms[i].MonsterFallSpeed+(1*Delta);
     end;
    end;
    if (State.Platforms[i].MonsterY+State.Platforms[i].MonsterYOfs+MonsterYOfs)>=CANVAS_HEIGHT then begin
     State.Platforms[i].Monster:=false;
    end;
   end else begin
    State.Platforms[i].MonsterXOfs:=State.Platforms[i].MonsterXOfs+(State.Platforms[i].MonsterDirection*Delta);
    if State.Platforms[i].MonsterXOfs<-(State.Platforms[i].MonsterWidth*0.25) then begin
     State.Platforms[i].MonsterXOfs:=-(State.Platforms[i].MonsterWidth*0.25);
     State.Platforms[i].MonsterDirection:=-State.Platforms[i].MonsterDirection;
    end else if State.Platforms[i].MonsterXOfs>(State.Platforms[i].Width-(State.Platforms[i].MonsterWidth*0.75)) then begin
     State.Platforms[i].MonsterXOfs:=State.Platforms[i].Width-(State.Platforms[i].MonsterWidth*0.75);
     State.Platforms[i].MonsterDirection:=-State.Platforms[i].MonsterDirection;
    end;
   end;
   //(2.0-(abs(frac(State.PartNowTime*0.1)-0.5)*4.0))*15.0;
  end;
 end;
end;

procedure SortClouds;
var i,j:integer;
begin
 i:=0;
 while i<(CountClouds-1) do begin
  if State.Clouds[State.CircleOrder[i]].s>State.Clouds[State.CircleOrder[i+1]].s then begin
   j:=State.CircleOrder[i];
   State.CircleOrder[i]:=State.CircleOrder[i+1];
   State.CircleOrder[i+1]:=j;
   if i>0 then begin
    dec(i);
   end else begin
    inc(i);
   end;
  end else begin
   inc(i);
  end;
 end;
end;

procedure CheckCollision(x1,y1,x2,y2:single;Phase:boolean);
 procedure Process(OfsX:single;First:boolean);
  function CheckCollisionMaps(ca,cb:PCollisionMap;cax1,cay1,cax2,cay2,cbx1,cby1,cbx2,cby2:single):boolean;
  var Collision:boolean;
   function IntersectRect(r1x,r1y,r1w,r1h,r2x,r2y,r2w,r2h:single):boolean;
   begin
    result:=((r2x+r2w)>r1x) and ((r2y+r2h)>r1y) and ((r1x+r1w)>r2x) and ((r1y+r1h)>r2y);
   end;
   function SweepAABBCheck(x00,y00,x01,y01,w0,h0,x10,y10,x11,y11,w1,h1:single):boolean;
   var AMinX,AMinY,AMaxX,AMaxY,BMinX,BMinY,BMaxX,BMaxY,VAx,VAy,VBx,VBy,Vx,Vy,U0x,U0y,U1x,U1y,U0,U1:single;
       Possible:boolean;
   begin
    result:=(IntersectRect(x00,y00,w0,h0,x10,y10,w1,h1) or IntersectRect(x01,y01,w0,h0,x11,y11,w1,h1)) or IntersectRect((x00+x01)*0.5,(y00+y01)*0.5,w0,h0,(x10+x11)*0.5,(y10+y11)*0.5,w1,h1);
    if not result then begin
     if IntersectRect(min(x00,x01),min(y00,y01),abs(x01-x00)+w0,abs(y01-y00)+h0,min(x10,x11),min(y10,y11),abs(x11-x10)+w1,abs(y11-y10)+h1) then begin
      AMinX:=x00;
      AMinY:=y00;
      AMaxX:=x00+w0;
      AMaxY:=y00+h0;
      BMinX:=x10;
      BMinY:=y10;
      BMaxX:=x10+w1;
      BMaxY:=y10+h1;
      VAx:=x01-x00;
      VAy:=y01-y00;
      VBx:=x11-x10;
      VBy:=y11-y10;
      Vx:=VBx-VAx;
      Vy:=VBy-VAy;
      U0x:=0;
      U0y:=0;
      U1x:=1;
      U1y:=1;
      Possible:=false;
      if Vx<0 then begin
       if AMaxX<BMinX then begin
        U0x:=(AMaxX-BMinX)/Vx;
        Possible:=true;
       end;
       if AMinX<BMaxX then begin
        U1x:=(AMinX-BMaxX)/Vx;
        Possible:=true;
       end;
      end else if Vx>0 then begin
       if AMinX>BMaxX then begin
        U0x:=(AMinX-BMaxX)/Vx;
        Possible:=true;
       end;
       if AMaxX>BMinX then begin
        U1x:=(AMaxX-BMinX)/Vx;
        Possible:=true;
       end;
      end else begin
       Possible:=not ((AMaxX<=BMinX) or (AMinX>=BMaxX));
      end;
      if Possible then begin
       Possible:=false;
       if Vy<0 then begin
        if AMaxY<BMinY then begin
         U0y:=(AMaxY-BMinY)/Vy;
         Possible:=true;
        end;
        if AMinY<BMaxY then begin
         U1y:=(AMinY-BMaxY)/Vy;
         Possible:=true;
        end;
       end else if Vy>0 then begin
        if AMinY>BMaxY then begin
         U0y:=(AMinY-BMaxY)/Vy;
         Possible:=true;
        end;
        if AMaxY>BMinY then begin
         U1y:=(AMaxY-BMinY)/Vy;
         Possible:=true;
        end;
       end else begin
        Possible:=not ((AMaxY<=BMinY) or (AMinY>=BMaxY));
       end;
       if Possible then begin
        U0:=max(U0x,U0y);
        U1:=min(U1x,U1y);
        if (U0<=U1) and ((U1>=0) and (U0<=1)) then begin
         result:=true;
        end;
       end;
      end;
     end;
    end;
   end;
   function Check(na,nb:PCollisionTreeNode):boolean;
   begin
    if assigned(na) and assigned(nb) then begin
     result:=SweepAABBCheck(cax1+na^.x1,cay1+na^.y1,cax2+na^.x1,cay2+na^.y1,na^.w,na^.h,cbx1+nb^.x1,cby1+nb^.y1,cbx2+nb^.x1 ,cby2+nb^.y1,nb^.w,nb^.h);
    end else begin
     result:=false;
    end;
   end;
   procedure ProcessA(na:PCollisionTreeNode);
    procedure ProcessB(nb:PCollisionTreeNode);
    var i:integer;
    begin
     if assigned(nb) then begin
      if nb^.HasContent then begin
       if Check(na,nb) then begin
        Collision:=true;
       end;
      end else begin
       if Check(na,nb) then begin
        for i:=0 to 3 do begin
         if Collision then begin
          break;
         end;
         if assigned(nb^.Children[i]) then begin
          ProcessB(nb^.Children[i]);
         end;
        end;
       end;
      end;
     end;
    end;
   var i:integer;
   begin
    if assigned(na) then begin
     if na^.HasContent then begin
      ProcessB(cb^.CollisionTreeRootNode);
     end else begin
      for i:=0 to 3 do begin
       if Collision then begin
        break;
       end;
       if assigned(na^.Children[i]) then begin
        ProcessA(na^.Children[i]);
       end;
      end;
     end;
    end;
   end;
  begin
   Collision:=false;
   if Check(ca^.CollisionTreeRootNode,cb^.CollisionTreeRootNode) then begin
    ProcessA(ca^.CollisionTreeRootNode);
   end;
   result:=Collision;
  end;
 var i,j:integer;
 begin
  for i:=0 to CountPlatforms-1 do begin
   if State.Platforms[i].Visible then begin
    if CheckCollisionMaps(State.Player.CollisionMap,State.Platforms[i].CollisionMap,x1+OfsX,y1,x2+OfsX,y2,State.Platforms[i].x,State.Platforms[i].y,State.Platforms[i].x,State.Platforms[i].y) then begin
     State.Platforms[i].Collision[Phase]:=true;
    end;
    if State.Platforms[i].Spring then begin
     if CheckCollisionMaps(State.Player.CollisionMap,State.Platforms[i].SpringCollisionMap,x1+OfsX,y1,x2+OfsX,y2,State.Platforms[i].SpringX,State.Platforms[i].SpringY+State.Platforms[i].SpringYOfs,State.Platforms[i].SpringX,State.Platforms[i].SpringY+State.Platforms[i].SpringYOfs) then begin
      State.Platforms[i].SpringCollision[Phase]:=true;
     end;
    end;
    if State.Platforms[i].Monster then begin
     if CheckCollisionMaps(State.Player.CollisionMap,State.Platforms[i].MonsterCollisionMap,x1+OfsX,y1,x2+OfsX,y2,State.Platforms[i].MonsterX,State.Platforms[i].MonsterY,State.Platforms[i].MonsterX,State.Platforms[i].MonsterY) then begin
      State.Platforms[i].MonsterCollision[Phase]:=true;
     end;
     if First and not State.Platforms[i].MonsterDead then begin
      for j:=0 to CountFires-1 do begin
       if State.Fires[j].Active then begin
        if CheckCollisionMaps(State.Fires[i].CollisionMap,State.Platforms[i].MonsterCollisionMap,State.Fires[j].x,State.Fires[j].y,State.Fires[j].x+State.Fires[j].dx,State.Fires[j].y+State.Fires[j].dy,State.Platforms[i].MonsterX+min(State.Platforms[i].MonsterDirection,0),State.Platforms[i].MonsterY,State.Platforms[i].MonsterX+max(State.Platforms[i].MonsterDirection,0),State.Platforms[i].MonsterY) then begin
         State.Platforms[i].MonsterFireCollision[Phase]:=true;
         State.Platforms[i].MonsterFireCollisionNo[Phase]:=j;
        end;
       end;
      end;
     end;
    end;
    if State.Platforms[i].Star then begin
     if CheckCollisionMaps(State.Player.CollisionMap,State.Platforms[i].StarCollisionMap,x1+OfsX,y1,x2+OfsX,y2,State.Platforms[i].StarX,State.Platforms[i].StarY,State.Platforms[i].StarX,State.Platforms[i].StarY) then begin
      State.Platforms[i].StarCollision[Phase]:=true;
     end;
    end;
   end;
  end;
 end;
var i:integer;
begin
 for i:=0 to CountPlatforms-1 do begin
  State.Platforms[i].Collision[Phase]:=false;
  State.Platforms[i].SpringCollision[Phase]:=false;
  State.Platforms[i].MonsterCollision[Phase]:=false;
  State.Platforms[i].MonsterFireCollision[Phase]:=false;
  State.Platforms[i].StarCollision[Phase]:=false;
 end;
 Process(0,true);
 if (x1>=(CANVAS_WIDTH-State.Player.Width)) or (x2>=(CANVAS_WIDTH-State.Player.Width)) then begin
  Process(-CANVAS_WIDTH,false);
 end;
 if (x1<0) or (x2<0) then begin
  Process(CANVAS_WIDTH,false);
 end;
end;

procedure Logic(DeltaTime:single);
var i,j,k:integer;
    DeltaTimeEx,InvGravity,Gravity,Diff,Dist,BestDist,ScrollSpeed,v,dx,dy:single;
    DoDead:boolean;
    PartNowTime:int64;
begin
 PartNowTime:=NowTime-PartStartTime;

 if InterlockedCompareExchange(State.DoInitTitleScreen,0,-1)<>0 then begin
  InitTitleScreen;
 end;
 if InterlockedCompareExchange(State.DoInitGameEx,0,-1)<>0 then begin
  InitGame;
 end; 
 if InterlockedCompareExchange(State.DoInitGame,0,-1)<>0 then begin
  InitPrestartScreen;
 end;

 case State.State of
  sPRESTARTSCREEN:begin  
   if PartNowTime>=6000 then begin
    InitGame;
   end;      
   exit;
  end;
  sPOSTGAMESCREEN:begin  
   if PartNowTime>=6000 then begin
    InitTitleScreen;
   end;      
   exit;
  end;
 end;

 if InterlockedCompareExchange(State.DoFire,0,-1)<>0 then begin
  if State.Player.Stars>0 then begin
   j:=-1;

   for i:=0 to CountFires-1 do begin
    if not State.Fires[i].Active then begin
     j:=i;
     break;
    end;
   end;
   if j<0 then begin
    BestDist:=0;
    for i:=0 to CountFires-1 do begin
     if State.Fires[i].Active then begin
      if BestDist<State.Fires[i].o then begin
       BestDist:=State.Fires[i].o;
       j:=i;
      end;
     end;
    end;
   end;
   if j>=0 then begin
    i:=j;
    dec(State.Player.Stars);
    State.Fires[i].Active:=true;
    State.Fires[i].x:=State.Player.x+((State.Player.Width-State.Fires[i].w)*0.5);
    State.Fires[i].y:=State.Player.y+((State.Player.Height-State.Fires[i].h)*0.5);
    State.Fires[i].dx:=(random*40)-20;
    State.Fires[i].dy:=-20;
    State.Fires[i].o:=0;
    if State.State=sGAME then begin
     SoundSystem.PlaySample(50,1,0.5,1);
    end;
    k:=-1;
    BestDist:=1e+12;
    for j:=0 to CountPlatforms-1 do begin
     if State.Platforms[j].Monster and not State.Platforms[j].MonsterDead then begin
      dx:=(State.Platforms[j].MonsterX+(State.Platforms[j].MonsterWidth*0.5))-(State.Player.x+(State.Player.Width*0.5));
      dy:=(State.Platforms[j].MonsterY+(State.Platforms[j].MonsterHeight*0.5))-(State.Player.y+(State.Player.Height*0.5));
      Dist:=sqr(dx)+sqr(dy);
      if (k<0) or (Dist<BestDist) then begin
       BestDist:=Dist;
       k:=j;
      end;
     end;
    end;
    if k>=0 then begin
     dx:=(State.Platforms[k].MonsterX+(State.Platforms[k].MonsterWidth*0.5))-(State.Player.x+(State.Player.Width*0.5));
     dy:=(State.Platforms[k].MonsterY+(State.Platforms[k].MonsterHeight*0.5))-(State.Player.y+(State.Player.Height*0.5));
     Dist:=sqrt(sqr(dx)+sqr(dy));
     if abs(Dist)>1e-8 then begin
      Dist:=1.0/Dist;
     end;
     Dist:=Dist*20.0;
     State.Fires[i].dx:=dx*Dist;
     State.Fires[i].dy:=dy*Dist;
    end;
   end;
  end;
 end;

 DeltaTimeEx:=DeltaTime*DeltaSpeedFactor;

 DoDead:=false;

 State.CollisionPhase:=not State.CollisionPhase;

 State.Player.OldX:=State.Player.x;
 State.Player.OldY:=State.Player.y;

 case State.State of
  sTITLESCREEN:begin
   while State.PlatformIndex<0 do begin
    inc(State.PlatformIndex,CountPlatforms);
   end;
   while State.PlatformIndex>=CountPlatforms do begin
    dec(State.PlatformIndex,CountPlatforms);
   end;
   BestDist:=1e16;
   if ((State.Player.State=psFALLING) and (State.Player.y>=State.Platforms[State.PlatformIndex].y)) or (State.Platforms[State.PlatformIndex].y>=CANVAS_HEIGHT) then begin
    for i:=0 to CountPlatforms-1 do begin
     if ((State.Player.State=psJUMPING) and (State.Player.y>=State.Platforms[i].y)) or ((State.Player.State=psFALLING) or ((State.Player.y<State.Platforms[i].y) and ((State.Platforms[i].y+State.Platforms[i].Height)<CANVAS_HEIGHT))) then begin
      Dist:=sqr((State.Platforms[i].x+(State.Platforms[i].Width*0.5))-(State.Player.x+(State.Player.Width*0.5)))+sqr((State.Platforms[i].y+(State.Platforms[i].Height*0.5))-(State.Player.y+(State.Player.Height*0.5)));
      if Dist<BestDist then begin
       BestDist:=Dist;
       State.PlatformIndex:=i;
      end;
     end;
    end;
   end;
   Diff:=State.Platforms[State.PlatformIndex].x-State.Player.x;
   if abs(Diff)>1 then begin
    State.Player.MoveSpeed:=Diff/abs(Diff);
   end;
   if State.Player.MoveSpeed<-1 then begin
    State.Player.MoveSpeed:=-1;
   end else if State.Player.MoveSpeed>1 then begin
    State.Player.MoveSpeed:=1;
   end;
   State.Player.CurrentMoveSpeed:=State.Player.CurrentMoveSpeed+((State.Player.MoveSpeed-State.Player.CurrentMoveSpeed)*(DeltaTimeEx*0.1));
   State.Player.x:=State.Player.x+(State.Player.CurrentMoveSpeed*DeltaTimeEx*5);
   State.Player.MoveSpeed:=State.Player.MoveSpeed-(State.Player.MoveSpeed*(sqr(DeltaTime)*100));
  end;
  sGAME:begin
   if State.Player.State<>psDEAD then begin
{$ifdef mobile}
    State.Player.x:=State.Player.x+(State.Player.Accelerometer*DeltaTimeEx);
//  State.Player.Accelerometer:=State.Player.Accelerometer-(State.Player.Accelerometer*0.0625*DeltaTimeEx);
{$else}
    State.Player.x:=State.Player.x+(State.Player.Accelerometer*DeltaTimeEx*5);
    State.Player.Accelerometer:=State.Player.Accelerometer-(State.Player.Accelerometer*0.0625*DeltaTimeEx);
{$endif}
   end;
  end;
 end;
 if abs(State.Player.x-State.Player.DirectionX)>5 then begin
  if State.Player.x<State.Player.DirectionX then begin
   State.Player.Direction:=-1;
  end else begin
   State.Player.Direction:=1;
  end;
  State.Player.DirectionX:=State.Player.x;
 end;
 while State.Player.x<0 do begin
  State.Player.x:=State.Player.x+CANVAS_WIDTH;
  State.Player.OldX:=State.Player.OldX+CANVAS_WIDTH;
  State.Player.DirectionX:=State.Player.DirectionX+CANVAS_WIDTH;
 end;
 while State.Player.x>=CANVAS_WIDTH do begin
  State.Player.x:=State.Player.x-CANVAS_WIDTH;
  State.Player.OldX:=State.Player.OldX-CANVAS_WIDTH;
  State.Player.DirectionX:=State.Player.DirectionX-CANVAS_WIDTH;
 end;

 InvGravity:=DeltaTimeEx*0.975;
 Gravity:=DeltaTimeEx*0.975;
 ScrollSpeed:=0;
 case State.Player.State of
  psJUMPING:begin
   ScrollSpeed:=State.Player.JumpSpeed;
   State.Player.y:=State.Player.y-(State.Player.JumpSpeed*DeltaTimeEx);
   if State.Player.JumpSpeed>Gravity then begin
    State.Player.JumpSpeed:=State.Player.JumpSpeed-InvGravity;
   end else begin
    State.Player.JumpSpeed:=0;
    State.Player.FallSpeed:=0;
    State.Player.State:=psFALLING;
   end;
  end;
  psFALLING:begin
   State.Player.y:=State.Player.y+(State.Player.FallSpeed*DeltaTimeEx);
   State.Player.FallSpeed:=State.Player.FallSpeed+Gravity;
   v:=(CANVAS_HEIGHT-State.Player.Height)-max((50-14)-(State.Player.JumpedHeight+State.Player.JumpedHeightEx),0);
   if State.Player.y>v then begin
    if (State.State<>STITLESCREEN) and (State.Player.JumpedHeight>(State.Player.Height*0.5)) then begin
     DoDead:=true;
    end else begin
     State.Player.JumpSpeed:=16;
     State.Player.MaxJumpSpeed:=16;
     State.Player.y:=v;
     State.Player.State:=psJUMPING;
     State.Player.IsSpringJump:=false;
     if State.State=sGAME then begin
      SoundSystem.PlaySample(20,1,0.5,1);
     end;
    end;
   end;
  end;
  psSPRING:begin
   if State.Platforms[State.Player.PlatformWithSpringIndex].SpringState=ssDOWN then begin
    State.Player.y:=State.Player.y+(State.Player.FallSpeed*DeltaTimeEx);
    State.Player.FallSpeed:=State.Player.FallSpeed+Gravity;
   end;
  end;
  psDEAD:begin
   ScrollSpeed:=-State.Player.FallSpeed;
{  if State.Player.JumpedHeight>=0 then begin
    State.Player.y:=State.Player.y+(State.Player.FallSpeed*DeltaTimeEx);
   end;}
   State.Player.FallSpeed:=State.Player.FallSpeed+Gravity;
  end;
 end;

 for i:=0 to CountPlatforms-1 do begin
  State.Platforms[i].SpringTouched:=false;
  State.Platforms[i].MonsterTouched:=false;
 end;

 CheckCollision(State.Player.OldX,State.Player.OldY,State.Player.x,State.Player.y,State.CollisionPhase);

 case State.Player.State of
  psJUMPING:begin
   for i:=0 to CountPlatforms-1 do begin
    if (State.Platforms[i].Kind=42) and (State.Platforms[i].Collision[State.CollisionPhase] {and (not State.Platforms[i].Collision[not State.CollisionPhase])} and (max(State.Player.y,State.Player.OldY)>=State.Platforms[i].y)) then begin
     State.Player.y:=max(State.Player.y,State.Platforms[i].y+(State.Platforms[i].Height-11));
     State.Platforms[i].Touched:=true;
     State.Platforms[i].TouchedFromDown:=true;
     State.Player.MaxJumpSpeed:=0;
     State.Player.JumpSpeed:=0;
     State.Player.FallSpeed:=0;
     State.Player.State:=psFALLING;
     if State.State=sGAME then begin
      SoundSystem.PlaySample(10,1,0.5,1);
     end;
     State.PlatformIndex:=i-1;
     if (State.Platforms[i].y+PlatformTopOfsY)<CANVAS_HEIGHT then begin
      DoDead:=false;
     end;
     break;
    end;
   end;
  end;
  psFALLING:begin
   for i:=0 to CountPlatforms-1 do begin
    if State.Platforms[i].Spring then begin
     if State.Platforms[i].SpringCollision[State.CollisionPhase] {and (State.Platforms[i].SpringCollision[not State.CollisionPhase])} and ((min(State.Player.y,State.Player.OldY)+(State.Player.Height-PlayerSubYHeadRoom))<=(State.Platforms[i].SpringY+State.Platforms[i].SpringYOfs)) then begin
      State.Platforms[i].SpringTouched:=true;
      State.Player.State:=psSPRING;
      State.Player.y:=(State.Platforms[i].SpringY+State.Platforms[i].SpringYOfs)-(State.Player.Height-PlayerSpringOfsY);
      State.Player.PlatformWithSpringIndex:=i;
      State.Platforms[i].SpringState:=ssDOWN;
      if ((State.Platforms[i].SpringY+State.Platforms[i].SpringYOfs)+SpringTopOfsY)<CANVAS_HEIGHT then begin
       DoDead:=false;
      end;
      break;
     end;
    end;
   end;
   if State.Player.State=psFALLING then begin
    for i:=0 to CountPlatforms-1 do begin
     if State.Platforms[i].Collision[State.CollisionPhase] {and (not State.Platforms[i].Collision[not State.CollisionPhase])} and ((min(State.Player.y,State.Player.OldY)+(State.Player.Height-PlayerSubYHeadRoom))<=State.Platforms[i].y) then begin
      State.Player.y:=min(State.Player.y,(State.Platforms[i].y+10)-State.Player.Height);
      State.Player.JumpSpeed:=24;
      State.Platforms[i].Touched:=true;
      State.Player.MaxJumpSpeed:=State.Player.JumpSpeed;
      State.Player.FallSpeed:=0;
      State.Player.State:=psJUMPING;
      State.Player.IsSpringJump:=false;
      State.PlatformIndex:=i+1;
      if State.State=sGAME then begin
       SoundSystem.PlaySample(20,1,0.5,1);
      end;
      if (State.Platforms[i].y+PlatformTopOfsY)<CANVAS_HEIGHT then begin
       DoDead:=false;
      end;
      break;
     end;
    end;
   end;
  end;
  psSPRING:begin
   i:=State.Player.PlatformWithSpringIndex;
   if ((i>=0) and (i<CountPlatforms)) and (State.Platforms[i].Spring and (State.Platforms[i].SpringCollision[State.CollisionPhase] or State.Platforms[i].SpringCollision[not State.CollisionPhase])) then begin
    State.Player.State:=psSPRING;
    State.Platforms[i].SpringTouched:=true;
    if State.Platforms[i].SpringState=ssDOWN then begin
     State.Platforms[i].SpringYOfs:=State.Platforms[i].SpringYOfs+abs(State.Player.y-State.Player.OldY);
     if State.Platforms[i].SpringYOfs>=MaxSpringYOfs then begin
      State.Platforms[i].SpringYOfs:=MaxSpringYOfs;
      State.Platforms[i].SpringState:=ssUP;
      State.Platforms[i].SpringYSpeed:=1;
      State.Player.FallSpeed:=0;
     end;
//   State.Player.y:=(State.Platforms[i].SpringY+State.Platforms[i].SpringYOfs)-(State.Player.Height-PlayerSpringOfsY);
    end;
   end else begin
    State.Player.State:=psFALLING;
    State.Player.FallSpeed:=State.Player.FallSpeed*0.25;
   end;
  end;
 end;

 case State.Player.State of
  psJUMPING,psSPRING:begin
   for i:=0 to CountPlatforms-1 do begin
    if (State.Platforms[i].Monster and not State.Platforms[i].MonsterDead) and (State.Platforms[i].MonsterCollision[State.CollisionPhase] and not State.Platforms[i].MonsterCollision[not State.CollisionPhase]) then begin
//   if (State.Platforms[i].Monster and not State.Platforms[i].MonsterDead) and (State.Platforms[i].MonsterCollision[State.CollisionPhase] {and (not State.Platforms[i].MonsterCollision[not State.CollisionPhase])} and (((max(State.Player.y,State.Player.OldY)-(State.Player.Height*0.5))<=(State.Platforms[i].MonsterY+(State.Platforms[i].MonsterHeight*0.5))))) then begin
     DoDead:=true;
     break;
    end;
   end;
  end;
  psFALLING:begin
   for i:=0 to CountPlatforms-1 do begin
    if (State.Platforms[i].Monster and not State.Platforms[i].MonsterDead) and (State.Platforms[i].MonsterCollision[State.CollisionPhase] and not State.Platforms[i].MonsterCollision[not State.CollisionPhase]) then begin
//  if (State.Platforms[i].Monster and not State.Platforms[i].MonsterDead) and (State.Platforms[i].MonsterCollision[State.CollisionPhase] {and (not State.Platforms[i].MonsterCollision[not State.CollisionPhase])} and ((min(State.Player.y,State.Player.OldY)+(State.Player.Height-PlayerSubYHeadRoom))<=State.Platforms[i].MonsterY)) then begin
     State.Platforms[i].MonsterX2:=State.Platforms[i].MonsterX;
     State.Platforms[i].MonsterDead:=true;
     State.Platforms[i].MonsterFallSpeed:=0;
     if State.State=sGAME then begin
      SoundSystem.PlaySample(70,1,0.5,1);
     end;
    end;
   end;
  end;
 end;
 for i:=0 to CountPlatforms-1 do begin
  if (State.Platforms[i].Monster and not State.Platforms[i].MonsterDead) and (State.Platforms[i].MonsterFireCollision[State.CollisionPhase] and not State.Platforms[i].MonsterFireCollision[not State.CollisionPhase]) then begin
   State.Platforms[i].MonsterX2:=State.Platforms[i].MonsterX;
   State.Platforms[i].MonsterDead:=true;
   State.Platforms[i].MonsterFallSpeed:=0;
   State.Fires[State.Platforms[i].MonsterFireCollisionNo[State.CollisionPhase]].Active:=false;
   if State.State=sGAME then begin
    SoundSystem.PlaySample(70,1,0.5,1);
   end;
  end;
 end;

 case State.Player.State of
  psJUMPING,psFALLING,psSPRING:begin
   for i:=0 to CountPlatforms-1 do begin
    if State.Platforms[i].Star and State.Platforms[i].StarCollision[State.CollisionPhase] then begin
     State.Platforms[i].Star:=false;
     if State.State=sGAME then begin
      SoundSystem.PlaySample(40,1,0.5,1);
     end;
     inc(State.Player.Stars);
    end;
   end;
  end;
 end;

 for i:=0 to CountPlatforms-1 do begin
  if State.Platforms[i].Spring then begin
   if not State.Platforms[i].SpringTouched then begin
    if State.Platforms[i].SpringState=ssDOWN then begin
     if State.Platforms[i].SpringYOfs>EPSILON then begin
      State.Platforms[i].SpringState:=ssUP;
     end else begin
      State.Platforms[i].SpringYOfs:=0;
      State.Platforms[i].SpringState:=ssNONE;
     end;
    end;
   end;
   if State.Platforms[i].SpringState=ssUP then begin
    State.Platforms[i].SpringYOfs:=State.Platforms[i].SpringYOfs-(DeltaTimeEx*State.Platforms[i].SpringYSpeed);
    State.Platforms[i].SpringYSpeed:=State.Platforms[i].SpringYSpeed+DeltaTimeEx;
    State.Player.y:=(State.Platforms[i].SpringY+State.Platforms[i].SpringYOfs)-(State.Player.Height-PlayerSpringOfsY);
    if (State.Player.PlatformWithSpringIndex=i) or State.Platforms[i].SpringTouched then begin
     State.Platforms[i].Touched:=true;
     State.Player.JumpSpeed:=64;
     State.Player.MaxJumpSpeed:=State.Player.JumpSpeed;
     State.Player.FallSpeed:=0;
     State.Player.State:=psJUMPING;
     State.Player.IsSpringJump:=true;
     if State.State=sGAME then begin
      SoundSystem.PlaySample(30,1,0.5,1);
     end;
     State.PlatformIndex:=i+1;
     for j:=0 to CountPlatforms-1 do begin
      State.Platforms[j].SpringTouched:=false;
     end;
    end;
    if State.Platforms[i].SpringYOfs<=0 then begin
     State.Platforms[i].SpringState:=ssNONE;
    end;
   end;
   if State.Platforms[i].SpringYOfs<0 then begin
    State.Platforms[i].SpringYOfs:=0;
   end else if State.Platforms[i].SpringYOfs>MaxSpringYOfs then begin
    State.Platforms[i].SpringYOfs:=MaxSpringYOfs;
   end;
  end;
 end;

 if DoDead then begin
  State.Player.DeadTime:=PartNowTime;
  State.Player.State:=psDEAD;
  if State.State=sGAME then begin
   SoundSystem.PlaySample(60,1,0.5,1);
  end;
 //State.Player.JumpedHeight:=0;
 end;

 case State.Player.State of
  psJUMPING,psFALLING,psSPRING:begin
   if State.Player.y<(CANVAS_HEIGHT*0.4) then begin
    Diff:=DeltaTimeEx*ScrollSpeed;
    State.Player.y:=State.Player.y+Diff;
    State.Player.JumpedHeight:=State.Player.JumpedHeight+Diff;
    State.Player.Score:=max(State.Player.Score,round(State.Player.JumpedHeight));
    MoveClouds(Diff*0.5);
    MoveFiresY(Diff);
    MovePlatforms(Diff);
   end;
  end;
  psDEAD:begin
// Diff:=max((((CANVAS_HEIGHT-State.Player.Height)*0.5)-State.Player.y)*0.25*DeltaTimeEx,DeltaTimeEx*ScrollSpeed);
   Diff:=DeltaTimeEx*ScrollSpeed;
{   if State.Player.JumpedHeight<0 then begin
    if State.Player.y>((CANVAS_HEIGHT+State.Player.Height)*0.5) then begin
     State.Player.y:=State.Player.y-abs(DeltaTimeEx*4);
    end else if State.Player.y<((CANVAS_HEIGHT-State.Player.Height)*0.5) then begin
     State.Player.y:=State.Player.y+abs(DeltaTimeEx*4);
    end;
   end else begin
    if State.Player.y>(CANVAS_HEIGHT*0.5) then begin
     State.Player.y:=State.Player.y+Diff;
     State.Player.JumpedHeight:=State.Player.JumpedHeight+Diff;
    end;
   end;}
   if State.Player.y>((CANVAS_HEIGHT+State.Player.Height)*0.5) then begin
    State.Player.y:=State.Player.y-abs(DeltaTimeEx*4);
   end else if State.Player.y<((CANVAS_HEIGHT-State.Player.Height)*0.5) then begin
    State.Player.y:=State.Player.y+abs(DeltaTimeEx*4);
   end;
   MoveClouds(Diff*0.5);
   MoveFiresY(Diff);
   MovePlatforms(Diff);
   State.Player.JumpedHeightEx:=State.Player.JumpedHeightEx+abs(Diff);
   if (PartNowTime-State.Player.DeadTime)>2500 then begin
    InitPostgameScreen;
    //InitTitleScreen;
   end;
  end;
 end;

 MoveMovingPlatforms(DeltaTimeEx);
 MoveFires(DeltaTimeEx);
 MoveMonsters(DeltaTimeEx);
end;

function Render(DeltaInc:int64):boolean;
const AlmostFullInvisible=0.1/256;
      AlmostFullVisible=1.0-AlmostFullInvisible;
      Chars:ansistring='ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_'#$ab#$b6#$bb;
var RenderState:PState;
    Fade,FadeEx:single;
    PartNowTime:int64;
 procedure DrawPlayer(px,py:single);
 var tx:single;
 begin
  case RenderState^.Player.State of
   psJUMPING:begin
    if RenderState^.Player.JumpSpeed<(RenderState^.Player.MaxJumpSpeed*0.125) then begin
     AnimationPlayerTransition.SetFrameTime(((1.0-(RenderState^.Player.JumpSpeed/(RenderState^.Player.MaxJumpSpeed*0.125)))*0.5),true);
     if RenderState^.Player.Direction<0 then begin
      AnimationPlayerTransition.Blit(px+RenderState^.Player.Width,py,0,-RenderState^.Player.Width,RenderState^.Player.Height,1,1,1,1*Fade);
      State.Player.CollisionMap:=AnimationPlayerTransition.GetCollisionMap(true);
     end else begin
      AnimationPlayerTransition.Blit(px,py,0,RenderState^.Player.Width,RenderState^.Player.Height,1,1,1,1*Fade);
      State.Player.CollisionMap:=AnimationPlayerTransition.GetCollisionMap(false);
     end;
    end else begin
     AnimationPlayerUp.SetFrameTime(PartNowTime*0.0025);
     if RenderState^.Player.Direction<0 then begin
      AnimationPlayerUp.Blit(px+RenderState^.Player.Width,py,0,-RenderState^.Player.Width,RenderState^.Player.Height,1,1,1,1*Fade);
      State.Player.CollisionMap:=AnimationPlayerUp.GetCollisionMap(true);
     end else begin
      AnimationPlayerUp.Blit(px,py,0,RenderState^.Player.Width,RenderState^.Player.Height,1,1,1,1*Fade);
      State.Player.CollisionMap:=AnimationPlayerUp.GetCollisionMap(true);
     end;
    end;
   end;
   psFALLING:begin
    if RenderState^.Player.FallSpeed<(RenderState^.Player.MaxJumpSpeed*0.125) then begin
     AnimationPlayerTransition.SetFrameTime((((RenderState^.Player.FallSpeed/(RenderState^.Player.MaxJumpSpeed*0.125)))*0.5)+0.5,true);
     if RenderState^.Player.Direction<0 then begin
      AnimationPlayerTransition.Blit(px+RenderState^.Player.Width,py,0,-RenderState^.Player.Width,RenderState^.Player.Height,1,1,1,1*Fade);
      State.Player.CollisionMap:=AnimationPlayerTransition.GetCollisionMap(true);
     end else begin
      AnimationPlayerTransition.Blit(px,py,0,RenderState^.Player.Width,RenderState^.Player.Height,1,1,1,1*Fade);
      State.Player.CollisionMap:=AnimationPlayerTransition.GetCollisionMap(false);
     end;
    end else begin
     AnimationPlayerDown.SetFrameTime(PartNowTime*0.0025);
     if RenderState^.Player.Direction<0 then begin
      AnimationPlayerDown.Blit(px+RenderState^.Player.Width,py,0,-RenderState^.Player.Width,RenderState^.Player.Height,1,1,1,1*Fade);
      State.Player.CollisionMap:=AnimationPlayerDown.GetCollisionMap(true);
     end else begin
      AnimationPlayerDown.Blit(px,py,0,RenderState^.Player.Width,RenderState^.Player.Height,1,1,1,1*Fade);
      State.Player.CollisionMap:=AnimationPlayerDown.GetCollisionMap(false);
     end;
    end;
   end;
   psSPRING:begin
    AnimationPlayerTransition.SetFrameTime(0,true);
    if RenderState^.Player.Direction<0 then begin
     AnimationPlayerTransition.Blit(px+RenderState^.Player.Width,py,0,-RenderState^.Player.Width,RenderState^.Player.Height,1,1,1,1*Fade);
     State.Player.CollisionMap:=AnimationPlayerTransition.GetCollisionMap(true);
    end else begin
     AnimationPlayerTransition.Blit(px,py,0,RenderState^.Player.Width,RenderState^.Player.Height,1,1,1,1*Fade);
     State.Player.CollisionMap:=AnimationPlayerTransition.GetCollisionMap(false);
    end;
   end;
   psDEAD:begin
    tx:=min(max((PartNowTime-RenderState^.Player.DeadTime)*0.001,0.0),1.0);
    AnimationPlayerDie.SetFrameTime(tx,true);
    AnimationPlayerDie.Blit(px,py,0,RenderState^.Player.Width,RenderState^.Player.Height,1,1,1,(1-((min(max(((PartNowTime-RenderState^.Player.DeadTime)*0.001)-1.0,0.0),1.0))))*Fade);
    State.Player.CollisionMap:=AnimationPlayerDie.GetCollisionMap(false);
   end;
  end;
 end;
 function TextWidth(const s:ansistring;Size:single):single;
 var i:integer;
     c:ansichar;
 begin
  result:=0;
  for i:=1 to length(s) do begin
   c:=s[i];
   result:=result+FontCharWidths[c];
  end;
  result:=(result*Size)/32;
 end;
 function TextHeight(const s:ansistring;Size:single):single;
 begin
  result:=Size/32;
 end;
 procedure DrawText(x,y:single;const s:ansistring;cr,cg,cb,ca,Size:single);
 var i:integer;
     c:ansichar;
     xf,sf:single;
 begin
  sf:=Size/32;
  xf:=0;
  for i:=1 to length(s) do begin
   c:=s[i];
   TextureFont.Blit(((byte(c) and $f)*32)+1,(((byte(c) shr 4) and $f)*32)+1,30,30,x+((xf+1)*sf),y+(1*sf),0,30*sf,30*sf,cr,cg,cb,ca);
   xf:=xf+FontCharWidths[c];
  end;
 end;
 procedure DrawValue(x,y:single;v:int64;cr,cg,cb,ca,Size:single);
 var w:int64;
     c:ansichar;
     xf,sf:single;
 begin
  sf:=Size/32;
  xf:=0;
  if v<0 then begin
   v:=-v;
   c:='-';
   TextureFont.Blit((byte(c) and $f)*32,((byte(c) shr 4) and $f)*32,32,32,x+(xf*sf),y,0,Size,Size,cr,cg,cb,ca);
   xf:=xf+FontCharWidths[c];
  end;
  if v=0 then begin
   c:='0';
   TextureFont.Blit((byte(c) and $f)*32,((byte(c) shr 4) and $f)*32,32,32,x+(xf*sf),y,0,Size,Size,cr,cg,cb,ca);
  end else begin
   w:=v;
   while w>0 do begin
    c:=char(byte((w mod 10)+48));
    w:=w div 10;
    xf:=xf+FontCharWidths[c];
    if (w=0) then begin
     break;
    end;
   end;
   w:=v;
   while w>0 do begin
    c:=char(byte((w mod 10)+48));
    w:=w div 10;
    xf:=xf-FontCharWidths[c];
    TextureFont.Blit((byte(c) and $f)*32,((byte(c) shr 4) and $f)*32,32,32,x+(xf*sf),y,0,Size,Size,cr,cg,cb,ca);
    if (w=0) then begin
     break;
    end;
   end;
  end;
 end;
var i,j,ToReadStateIndex:integer;
    TitleScreenFactor,tx,ty,sw,sh,f,v:single;
    s:ansistring;
begin
 result:=false;
 if InterlockedCompareExchange(ReadStateIndexLocked,-1,0)=0 then begin
  ToReadStateIndex:=ReadStateIndex;
  InterlockedExchange(ReadStateIndexLocked,0);
  ToReadStateIndex:=ToReadStateIndex and $f;
  if InterlockedCompareExchange(RenderStateLocks[ToReadStateIndex],-1,0)=0 then begin
   RenderState:=@RenderStates[ToReadStateIndex];
   if assigned(RenderState) and (RenderState^.ReadyToRender<>0) then begin

    for i:=1 to 15 do begin
     j:=(ToReadStateIndex-i) and $f;
     if InterlockedCompareExchange(RenderStateLocks[j],-1,0)=0 then begin
      if (RenderStates[j].ReadyToRender<>0) and (RenderStates[j].FrameTime<RenderState^.FrameTime) then begin
       InterlockedExchange(RenderStates[j].ReadyToRender,0);
      end;
      InterlockedExchange(RenderStateLocks[j],0);
     end;
    end;

    PartNowTime:=RenderState^.PartNowTime;

    if DoUpload then begin
     DoUpload:=false;
     ProcessUpload;
    end;

    case RenderState^.Player.State of
     psDEAD:begin
      Fade:=1.0-(sqr(sqr(min(max((PartNowTime-RenderState^.Player.DeadTime)*0.001,0.0),1.0))));
     end;
     else begin
      if RenderState^.State=sTITLESCREEN then begin
       Fade:=min(max(PartNowTime*0.001,0.0),1.0);
      end else begin
       Fade:=1;
      end;
     end;
    end;

    glViewPort(0,0,SCREEN_WIDTH,SCREEN_HEIGHT);
    glClearColor(0,0,0,1.0);
    glClear(GL_COLOR_BUFFER_BIT);

    glDisable(GL_CULL_FACE);
    glDisable(GL_DEPTH_TEST);
    glDepthMask(GL_FALSE);

    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);

    if RenderState^.State=sTITLESCREEN then begin
     TitleScreenFactor:=min(max(cos(PartNowTime*0.000125)*8,0),1);
    end else begin
     TitleScreenFactor:=0.0;
    end;

    SwitchToTextureBlit;
    QuadVBOBegin;
    begin
     if (RenderState^.State in [sTITLESCREEN,sGAME]) and ((RenderState^.State=sGAME) or (TitleScreenFactor<AlmostFullVisible)) then begin
      begin
       f:=Fade;
       if RenderState^.State=sTITLESCREEN then begin
        f:=f*min(max((PartNowTime*0.001)-1,0.0),1.0);
       end;
       if f>AlmostFullInvisible then begin
        if f>=AlmostFullVisible then begin
         glDisable(GL_BLEND);
         TextureBG1.Blit(0,0,TextureBG1.Width,TextureBG1.Height,0,0,0,CANVAS_WIDTH,CANVAS_HEIGHT,1,1,1,f);
         glEnable(GL_BLEND);
        end else begin
         TextureBG1.Blit(0,0,TextureBG1.Width,TextureBG1.Height,0,0,0,CANVAS_WIDTH,CANVAS_HEIGHT,1,1,1,f);
        end;
       end;
      end;
      begin
       SortClouds;
       for i:=0 to CountClouds-1 do begin
        j:=RenderState^.CircleOrder[i];
   //   DrawCircle(RenderState^.Clouds[j].x,RenderState^.Clouds[j].y,0,RenderState^.Clouds[j].r*((1-0.06125)+(cos((PartNowTime+RenderState^.Clouds[j].r+(j*250))*0.0025)*0.06125)),RenderState^.Clouds[j].a*Fade);
        TextureClouds[RenderState^.Clouds[j].k].Blit(0,0,TextureClouds[RenderState^.Clouds[j].k].Width,TextureClouds[RenderState^.Clouds[j].k].Height,RenderState^.Clouds[j].x,RenderState^.Clouds[j].y,0,RenderState^.Clouds[j].w,RenderState^.Clouds[j].h,1,1,1,RenderState^.Clouds[j].a*Fade);
       end;
      end;
      begin
       SortPlatforms;
       for j:=0 to CountPlatforms-1 do begin
        i:=RenderState^.PlatformOrder[j];
        if RenderState^.Platforms[i].Visible then begin
         if RenderState^.Platforms[i].Monster then begin
          if RenderState^.Platforms[i].MonsterDead then begin
           AnimationMonsterDie.SetFrameTime(RenderState^.Platforms[i].MonsterDeadTime,true);
           if State.Platforms[i].MonsterDirection<0 then begin
            AnimationMonsterDie.Blit(RenderState^.Platforms[i].MonsterX2+State.Platforms[i].MonsterXOfs+RenderState^.Platforms[i].MonsterWidth,RenderState^.Platforms[i].MonsterY+State.Platforms[i].MonsterYOfs+MonsterYOfs,0,-RenderState^.Platforms[i].MonsterWidth,RenderState^.Platforms[i].MonsterHeight,1,1,1,1*Fade);
            RenderState^.Platforms[i].MonsterCollisionMap:=AnimationMonsterDie.GetCollisionMap(true);
           end else begin
            AnimationMonsterDie.Blit(RenderState^.Platforms[i].MonsterX2+State.Platforms[i].MonsterXOfs,RenderState^.Platforms[i].MonsterY+State.Platforms[i].MonsterYOfs+MonsterYOfs,0,RenderState^.Platforms[i].MonsterWidth,RenderState^.Platforms[i].MonsterHeight,1,1,1,1*Fade);
            RenderState^.Platforms[i].MonsterCollisionMap:=AnimationMonsterDie.GetCollisionMap(false);
           end;
          end else begin
           AnimationMonsterWalk.SetFrameTime(PartNowTime*0.005);
           if State.Platforms[i].MonsterDirection<0 then begin
            AnimationMonsterWalk.Blit(RenderState^.Platforms[i].MonsterX+State.Platforms[i].MonsterXOfs+RenderState^.Platforms[i].MonsterWidth,RenderState^.Platforms[i].MonsterY+State.Platforms[i].MonsterYOfs+MonsterYOfs,0,-RenderState^.Platforms[i].MonsterWidth,RenderState^.Platforms[i].MonsterHeight,1,1,1,1*Fade);
            RenderState^.Platforms[i].MonsterCollisionMap:=AnimationMonsterWalk.GetCollisionMap(true);
           end else begin
            AnimationMonsterWalk.Blit(RenderState^.Platforms[i].MonsterX+State.Platforms[i].MonsterXOfs,RenderState^.Platforms[i].MonsterY+State.Platforms[i].MonsterYOfs+MonsterYOfs,0,RenderState^.Platforms[i].MonsterWidth,RenderState^.Platforms[i].MonsterHeight,1,1,1,1*Fade);
            RenderState^.Platforms[i].MonsterCollisionMap:=AnimationMonsterWalk.GetCollisionMap(false);
           end;
          end;
         end;
         if RenderState^.Platforms[i].Spring then begin
          TextureSpring.Blit(0,0,TextureSpring.Width,TextureSpring.Height,RenderState^.Platforms[i].SpringX,RenderState^.Platforms[i].SpringY+RenderState^.Platforms[i].SpringYOfs,0,RenderState^.Platforms[i].SpringWidth,RenderState^.Platforms[i].SpringHeight,1,1,1,1*Fade);
         end;
         if RenderState^.Platforms[i].Star then begin
          AnimationStar.SetFrameTime(PartNowTime*0.005);
          AnimationStar.Blit(RenderState^.Platforms[i].StarX,RenderState^.Platforms[i].StarY,0,RenderState^.Platforms[i].StarWidth,RenderState^.Platforms[i].StarHeight,1,1,1,1*Fade);
         end;
         case RenderState^.Platforms[i].Kind of
          42:begin
           if RenderState^.Platforms[i].TouchedFromDown then begin
            TextureImperviousPlatformTouched.Blit(0,0,TextureImperviousPlatformTouched.Width,TextureImperviousPlatformTouched.Height,RenderState^.Platforms[i].x,RenderState^.Platforms[i].y,0,RenderState^.Platforms[i].Width,RenderState^.Platforms[i].Height,1,1,1,1*Fade);
            RenderState^.Platforms[i].CollisionMap:=TextureImperviousPlatformTouched.GetCollisionMap(false);
           end else begin
            TextureImperviousPlatformNormal.Blit(0,0,TextureImperviousPlatformNormal.Width,TextureImperviousPlatformNormal.Height,RenderState^.Platforms[i].x,RenderState^.Platforms[i].y,0,RenderState^.Platforms[i].Width,RenderState^.Platforms[i].Height,1,1,1,1*Fade);
            RenderState^.Platforms[i].CollisionMap:=TextureImperviousPlatformNormal.GetCollisionMap(false);
           end;
          end;
          else begin
           if RenderState^.Platforms[i].Touched then begin
            TextureSolidPlatformAwake.Blit(0,0,TextureSolidPlatformAwake.Width,TextureSolidPlatformAwake.Height,RenderState^.Platforms[i].x,RenderState^.Platforms[i].y,0,RenderState^.Platforms[i].Width,RenderState^.Platforms[i].Height,1,1,1,1*Fade);
            RenderState^.Platforms[i].CollisionMap:=TextureSolidPlatformAwake.GetCollisionMap(false);
           end else begin
            TextureSolidPlatformAsleep.Blit(0,0,TextureSolidPlatformAsleep.Width,TextureSolidPlatformAsleep.Height,RenderState^.Platforms[i].x,RenderState^.Platforms[i].y,0,RenderState^.Platforms[i].Width,RenderState^.Platforms[i].Height,1,1,1,1*Fade);
            RenderState^.Platforms[i].CollisionMap:=TextureSolidPlatformAsleep.GetCollisionMap(false);
           end;
          end;
         end;
        end;
       end;
      end;
      begin
       if ((CANVAS_HEIGHT-50)+(State.Player.JumpedHeight+State.Player.JumpedHeightEx))<CANVAS_HEIGHT then begin
        TextureFloorFront.Blit(0,0,TextureFloorFront.Width,TextureFloorFront.Height,0,(CANVAS_HEIGHT-50)+(State.Player.JumpedHeight+State.Player.JumpedHeightEx),0,TextureFloorFront.Width,TextureFloorFront.Height,1,1,1,1*Fade);
       end;
      end;
      begin
       for i:=0 to CountFires-1 do begin
        if State.Fires[i].Active then begin
         AnimationFire.SetFrameTime(PartNowTime*0.01);
         AnimationFire.Blit(State.Fires[i].x,State.Fires[i].y,0,AnimationFire.Width,AnimationFire.Height,1,1,1,1*Fade);
         State.Fires[i].CollisionMap:=AnimationFire.GetCollisionMap(false);
        end;
       end;
      end;
      begin
       if RenderState^.Player.State=psDEAD then begin
        tx:=RenderState^.Player.x+(((CANVAS_WIDTH*0.5)-RenderState^.Player.x)*min(max((PartNowTime-RenderState^.Player.DeadTime)*0.001,0.0),1.0));
       end else begin
        tx:=RenderState^.Player.x;
       end;
       while tx<0 do begin
        tx:=tx+CANVAS_WIDTH;
       end;
       if (((tx+RenderState^.Player.Width)>=0)) and (tx<CANVAS_WIDTH) then begin
        DrawPlayer(tx,RenderState^.Player.y);
       end;
       if ((tx+RenderState^.Player.Width)-CANVAS_WIDTH)>=0 then begin
        DrawPlayer(tx-CANVAS_WIDTH,RenderState^.Player.y);
       end;
       if (tx+CANVAS_WIDTH)<CANVAS_WIDTH then begin
        DrawPlayer(tx+CANVAS_WIDTH,RenderState^.Player.y);
       end;
      end;
      begin
       if RenderState^.State=sGAME then begin
        if RenderState^.Player.State=psDEAD then begin
         FadeEx:=1.0-(sqr(sqr(min(max(((PartNowTime-RenderState^.Player.DeadTime)*0.001)-1.0,0.0),1.0))));
        end else begin
         FadeEx:=1;
        end;
        DrawText(10,10,'Score: ',1.0-Fade,1.0-Fade,1.0-Fade,1*FadeEx,24);
        DrawValue(72,10,RenderState^.Player.Score,1.0-Fade,1.0-Fade,1.0-Fade,1*FadeEx,24);
        DrawText(180,10,'Stars: ',1.0-Fade,1.0-Fade,1.0-Fade,1*FadeEx,24);
        DrawValue(180+62,10,RenderState^.Player.Stars,1.0-Fade,1.0-Fade,1.0-Fade,1*FadeEx,24);
       end;
      end;
      begin
{      if RenderState^.Player.State=psDEAD then begin
        FadeEx:=sqr(sqr(min(max(((PartNowTime-RenderState^.Player.DeadTime)*0.001),0.0),1.0)));
        FadeEx:=FadeEx*(1.0-sqr(sqr(min(max((((PartNowTime-RenderState^.Player.DeadTime)*0.001)-1.0),0.0),1.0))));
        TextureGameOver.Blit(0,0,TextureGameOver.Width,TextureGameOver.Height,0,0,0,CANVAS_WIDTH,CANVAS_HEIGHT,1,1,1,FadeEx);
       end;}
      end;
     end;
     case RenderState^.State of
      sPRESTARTSCREEN:begin  
       if PartNowTime<2000 then begin
        FadeEx:=1-sqr(1-(PartNowTime*0.0005));
       end else if PartNowTime<4000 then begin
        FadeEx:=1;
       end else if PartNowTime<=6000 then begin
        FadeEx:=sqr(1-((PartNowTime-4000)*0.0005));
       end else begin
        FadeEx:=0;
       end;      
//     FadeEx:=1.0-sqr(sqr(1.0-min(max(PartNowTime*0.001,0.0),1.0)));
       TexturePrestartScreen.Blit(0,0,TexturePrestartScreen.Width,TexturePrestartScreen.Height,0,0,0,CANVAS_WIDTH,CANVAS_HEIGHT,1,1,1,FadeEx);
      end;
      sPOSTGAMESCREEN:begin
       if PartNowTime<2000 then begin
        FadeEx:=1-sqr(1-(PartNowTime*0.0005));
       end else if PartNowTime<4000 then begin
        FadeEx:=1;
       end else if PartNowTime<=6000 then begin
        FadeEx:=sqr(1-((PartNowTime-4000)*0.0005));
       end else begin
        FadeEx:=0;
       end;      
//     FadeEx:=1.0-sqr(sqr(1.0-min(max(PartNowTime*0.001,0.0),1.0)));
       TexturePostgameScreen.Blit(0,0,TexturePostgameScreen.Width,TexturePostgameScreen.Height,0,0,0,CANVAS_WIDTH,CANVAS_HEIGHT,1,1,1,FadeEx);
      end;
     end;
     if (TitleScreenFactor>AlmostFullInvisible) and (RenderState^.State=sTITLESCREEN) then begin
      f:=TitleScreenFactor*Fade;
      if f>AlmostFullInvisible then begin
       begin
        if f>=AlmostFullVisible then begin
         glDisable(GL_BLEND);
         TextureTitleScreen.Blit(0,0,TextureTitleScreen.Width,TextureTitleScreen.Height,0,0,0,CANVAS_WIDTH,CANVAS_HEIGHT,1,1,1,f);
         glEnable(GL_BLEND);
        end else begin
         TextureTitleScreen.Blit(0,0,TextureTitleScreen.Width,TextureTitleScreen.Height,0,0,0,CANVAS_WIDTH,CANVAS_HEIGHT,1,1,1,f);
        end;
       end;
   {   begin
   //   TextureTitle.Blit(0,0,TextureTitle.Width,TextureTitle.Height,4,4,0,CANVAS_WIDTH,CANVAS_HEIGHT,0.5,0.5,0.5,0.5*f);
        TextureTitle.Blit(0,0,TextureTitle.Width,TextureTitle.Height,0,0,0,CANVAS_WIDTH,CANVAS_HEIGHT,1,1,1,f);
       end;{}
       begin
        tx:=(cos(PartNowTime*0.001)*64)+((CANVAS_WIDTH-(TextureTitleScreenStart.Width*0.8))*0.5);
        ty:=(sin(PartNowTime*0.0005)*64)+((CANVAS_HEIGHT-(TextureTitleScreenStart.Height*0.8))*0.4);
{       sw:=1.125+(cos(PartNowTime*0.00125)*0.125);
        sh:=1.125+(sin(PartNowTime*0.0025)*0.125);}
        sw:=(1.125+(cos(PartNowTime*0.00125)*0.125))*0.8;
        sh:=sw;
        tx:=tx+((CANVAS_WIDTH-(CANVAS_WIDTH*sw))*0.0625);
        ty:=ty+((CANVAS_HEIGHT-(CANVAS_HEIGHT*sh))*0.0625);
   //  TextureTitle2.Blit(0,0,TextureTitle2.Width,TextureTitle2.Height,tx+4,ty+4,0,(CANVAS_WIDTH*sw)*0.5,(CANVAS_HEIGHT*sh)*0.125,0.5,0.5,0.5,0.5*f);
        TextureTitleScreenStart.Blit(0,0,TextureTitleScreenStart.Width,TextureTitleScreenStart.Height,tx+0,ty+0,0,TextureTitleScreenStart.Width*sw,TextureTitleScreenStart.Height*sh,1,1,1,f);
//      TextureTitle2.Blit(0,0,TextureTitle2.Width,TextureTitle2.Height,tx+0,ty+0,0,(CANVAS_WIDTH*sw)*0.5,(CANVAS_HEIGHT*sh)*0.125,1,1,1,f);
       end;
      end;
     end;
     begin
      if RenderState^.Player.State=psDEAD then begin
       FadeEx:=1.0-(sqr(sqr(min(max(((PartNowTime-RenderState^.Player.DeadTime)*0.001)-1.0,0.0),1.0))));
      end else begin
       FadeEx:=1;
      end;
{     DrawText(350,10,'FPS: ',1.0-Fade,1.0-Fade,1.0-Fade,1*FadeEx,24);
      DrawValue(350+45,10,FPS,1.0-Fade,1.0-Fade,1.0-Fade,1*FadeEx,24);{}
     end;
    end;     

{   SwitchToSolidBlit;
    SolidBlit(16,16,0,2,CANVAS_HEIGHT-32,0.975,0.975,0.975,0.875);
    SolidBlit(CANVAS_WIDTH-18,16,0,2,CANVAS_HEIGHT-32,0.975,0.975,0.975,0.875);
    SolidBlit(16,16,0,CANVAS_WIDTH-32,2,0.975,0.975,0.975,0.875);
    SolidBlit(16,CANVAS_HEIGHT-18,0,CANVAS_WIDTH-32,2,0.975,0.975,0.975,0.875);
    SolidBlit(18,18,0,CANVAS_WIDTH-36,CANVAS_HEIGHT-36,0,0,0,0.95);
//  SolidBlit(0,0,0,CANVAS_WIDTH,CANVAS_HEIGHT,0,0,0,0.875);
    SwitchToTextureBlit;

    if (PartNowTime mod 20000)>10000 then begin
     ty:=38+(sin(PartNowTime*0.005)*10);
     DrawText((500-TextWidth('Highscores',ty))*0.5,48+((48-ty)*0.5),'Highscores',1.0,0.875,abs(0.5+(cos(PartNowTime*0.01)*0.5)),1.0,ty);
     for i:=1 to 20 do begin
      tx:=24+(cos((PartNowTime*0.001)+(i*pi/10))*16);
      ty:=24+(sin((PartNowTime*0.002)+(i*pi/10))*8);
      f:=abs(0.25+(cos((PartNowTime*0.01)+(i*pi/10))*0.25));
      v:=abs(0.25+(cos((PartNowTime*0.001)+(i*pi/10))*0.25));
      s:=IntToStr(i)+'.';
      DrawText(tx+72-TextWidth(s,ty),96+(i*32)+((32-ty)*0.5),s,1.0,0.5+f,0.125+v,1.0,ty);
      DrawText(tx+80,96+(i*32)+((32-ty)*0.5),'BERO',1.0,0.5+f,0.125+v,1.0,ty);
      f:=abs(0.25+(sin((PartNowTime*0.01)+(i*pi/10))*0.25));
      v:=abs(0.25+(sin((PartNowTime*0.001)+(i*pi/10))*0.25));
      s:=IntToStr(((21-i)*14107)+(((21-i)*6689) div 2));
      DrawText(CANVAS_WIDTH-(tx+38+TextWidth(s,ty)),96+(i*32)+((32-ty)*0.5),s,1.0-f,0.5+f,0.125+v,1.0,ty);
     end;
    end else begin
     ty:=38+(sin(PartNowTime*0.005)*10);
     DrawText((500-TextWidth('Enter your name',ty))*0.5,48+((48-ty)*0.5),'Enter your name',1.0,0.875,abs(0.5+(cos(PartNowTime*0.01)*0.5)),1.0,ty);
     DrawText(32,300,'BERO_',abs(0.875+(cos(PartNowTime*0.01)*0.125)),0.875,0.875,1.0,48);
     for i:=0 to length(Chars)-1 do begin
      tx:=((i mod 8)*56)+30;
      ty:=((i div 8)*60)+480;
      f:=abs(0.25+(sin((PartNowTime*0.01)+(i*pi/10))*0.25));
      v:=abs(0.25+(sin((PartNowTime*0.001)+(i*pi/10))*0.25));
      SwitchToSolidBlit;
      SolidBlit(tx,ty,0,48,48,f*0.5,v*0.5,0.25-(f*0.25),0.875);
      SwitchToTextureBlit;
      DrawText(tx+(24-(TextWidth(Chars[i+1],48)*0.5)),ty,Chars[i+1],1.0-(f+v),0.5+f,0.5+v,1.0,48);
     end;
    end;    }

    QuadVBOEnd;
    SwitchToNothing;

    if DeltaInc<>0 then begin
     if DeltaInc<0 then begin
      DeltaInc:=0;
     end else if DeltaInc>1000 then begin
      DeltaInc:=1000;
     end;
     FPSex:=FPSex+(((1000/DeltaInc)-FPSex)*(1.0-power(FPSFilterFactor,DeltaInc)));
     FPS:=round(FPSex);
    end;

    FirstFrameWasDrawn:=true;

    result:=true;

    InterlockedExchange(RenderState^.ReadyToRender,0);
   end;
   InterlockedExchange(RenderStateLocks[ToReadStateIndex],0);
  end;
 end;
end;

procedure UpdateSound;
begin
 if FirstFrameWasDrawn then begin
  if State.CurrentTrack<>State.LastTrack then begin
   if (State.LastTrack>=0) and (State.LastTrack<255) then begin
    SoundSystem.StopXMTrack(State.LastTrack);
   end;
   State.LastTrack:=State.CurrentTrack;
   if (State.CurrentTrack>=0) and (State.CurrentTrack<255) then begin
    if not SoundSystem.IsXMTrackPlaying(State.CurrentTrack) then begin
     SoundSystem.PlayXMTrack(State.CurrentTrack);
    end;
   end;
  end else if not SoundRunning then begin
   if (State.CurrentTrack>=0) and (State.CurrentTrack<255) then begin
    if not SoundSystem.IsXMTrackPlaying(State.CurrentTrack) then begin
     SoundSystem.PlayXMTrack(State.CurrentTrack);
    end;
   end;
   SoundRunning:=true;
  end;
 end;
end;

procedure Process(DeltaInc:int64);
var i,Index:integer;
    DoBreak:boolean;
begin
 if DeltaInc<0 then begin
  DeltaInc:=0;
 end else if DeltaInc>1000 then begin
  DeltaInc:=1000;
 end;
 Delta:=Delta+(DeltaInc*0.001);
 while Delta>=ScreenInvHz do begin
  Delta:=Delta-ScreenInvHz;
  Logic(ScreenInvHz);
 end;
 if Delta>0 then begin
  Logic(Delta);
  Delta:=0;
 end;
 State.PartNowTime:=NowTime-PartStartTime;
 begin
  State.FrameTime:=GetNowTime;
  for i:=8 to $f do begin
   Index:=(StateIndex-i) and $f;
   if InterlockedCompareExchange(RenderStateLocks[Index],-1,0)=0 then begin
    if (RenderStates[Index].ReadyToRender<>0) and (RenderStates[Index].FrameTime<State.FrameTime) then begin
     InterlockedExchange(RenderStates[Index].ReadyToRender,0);
    end;
    InterlockedExchange(RenderStateLocks[Index],0);
   end;
  end;
  StateIndex:=(StateIndex+1) and $f;
  for i:=0 to $f do begin
   DoBreak:=false;
   if InterlockedCompareExchange(RenderStateLocks[StateIndex],-1,0)=0 then begin
    if RenderStates[StateIndex].ReadyToRender=0 then begin
     RenderStates[StateIndex]:=State;
     InterlockedExchange(RenderStates[StateIndex].ReadyToRender,-1);
     if InterlockedCompareExchange(ReadStateIndexLocked,-1,0)=0 then begin
      InterlockedExchange(ReadStateIndex,StateIndex);
      InterlockedExchange(ReadStateIndexLocked,0);
     end;
     DoBreak:=true;
    end;
    InterlockedExchange(RenderStateLocks[StateIndex],0);
    if DoBreak then begin
     break;
    end;
   end;
   StateIndex:=(StateIndex+1) and $f;
  end;
 end;
end;

procedure Suspend;
begin
 FirstFrameWasDrawn:=false;
 SoundRunning:=false;
 if (State.CurrentTrack>=0) and (State.CurrentTrack<255) then begin
  if SoundSystem.IsXMTrackPlaying(State.CurrentTrack) then begin
   SoundSystem.StopXMTrack(State.CurrentTrack);
  end;
 end;
end;

procedure Resume;
begin
 SoundRunning:=false;
 FirstFrameWasDrawn:=false;
end;

procedure AccelerometerChange(x,y,z:single);
begin
{$ifdef mobile}
 State.Player.Accelerometer:=x;
{$else}
 if (longword(pointer(@State.Player.Accelerometer)^) shr 31)<>(longword(pointer(@x)^) shr 31) then begin
  State.Player.Accelerometer:=x;
 end else begin
  State.Player.Accelerometer:=State.Player.Accelerometer+x;
 end;
{$endif}
end;

procedure Touch(x,y:single);
begin
 case State.State of
  sTITLESCREEN:begin
   InterlockedExchange(State.DoInitGame,-1);
  end;
  sPRESTARTSCREEN:begin
   InterlockedExchange(State.DoInitGameEx,-1);
  end;
  sGAME:begin
   InterlockedExchange(State.DoFire,-1);
  end;
  sPOSTGAMESCREEN:begin
   InterlockedExchange(State.DoInitTitleScreen,-1);
  end;
 end;
end;

procedure BackToTitleScreen;
begin
 InterlockedExchange(State.DoInitTitleScreen,-1);
end;

end.
