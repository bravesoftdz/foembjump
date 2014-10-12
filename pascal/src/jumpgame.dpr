{$ifdef fpc}
 {$mode delphi}
 {$h+}
{$endif}
{$ifndef mobile}
program jumpgame;
{$ifdef win32}
 {$apptype gui}
{$endif}
{$else}
library jumpgame;
{$endif}
{$warnings off}

uses
{$ifdef android}
//androidthreads in 'androidthreads.pas',
  jni in 'jni.pas',
  log in 'log.pas',
  gles20 in 'gles20.pas',
{$else}
{$ifdef unix}
  cthreads,
{$endif}
{$endif}
  SysUtils,
  Classes,
{$ifdef win32}
  Windows,
{$endif}
{$ifndef mobile}
  sdl in 'SDL\sdl.pas',
  gl in 'OpenGL\gl.pas',
  glext in 'OpenGL\glext.pas',
  logger in 'SDL\logger.pas',
  moduleloader in 'SDL\moduleloader.pas',
{$endif}
  RenderBase in 'RenderBase.pas',
  GLUtils in 'GLUtils.pas',
  BeRoCriticalSection in 'BeRoCriticalSection.pas',
  GameCore in 'GameCore.pas',
  BeRoVectorCanvas in 'BeRoVectorCanvas.pas',
  BeRoSoundSystem in 'BeRoSoundSystem.pas',
  BeRoXM in 'BeRoXM.pas',
  MathUtils in 'MathUtils.pas',
  dataunit in 'tools\packdata\dataunit.pas',
  DataManager in 'DataManager.pas';

{$ifdef mobile}
const Loaded:boolean=false;

      curClass:JClass=nil;
      mInstance:JFieldID=nil;

      RenderNowTime:int64=0;
      RenderLastTime:int64=0;

procedure Java_com_bero_games_foembjump_JumpGame_nativeCreate(Env:PJNIEnv;this:JObject;SampleRate,Channels,Bits,BufferSamples:JInt);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
 Log.__android_log_write(Log.ANDROID_LOG_DEBUG,'libjumpgame','Entering nativeCreate . . .');
 if not Loaded then begin
  Loaded:=true;
  InitializeData;
 end;
 SoundSystem:=TBeRoSoundSystem.Create(SampleRate,Channels,Bits,BufferSamples);
 GameCore.Init;
 StartTime:=GetNowTime;
 NowTime:=GetNowTime-StartTime;
 LastTime:=NowTime;
 RenderNowTime:=GetNowTime-StartTime;
 RenderLastTime:=RenderNowTime;
 if State.State=sTITLESCREEN then begin
  InitTitleScreen;
  PartStartTime:=NowTime;
 end;
 Log.__android_log_write(Log.ANDROID_LOG_DEBUG,'libjumpgame','Leaving nativeCreate . . .');
end;

procedure Java_com_bero_games_foembjump_JumpGame_nativeDestroy(Env:PJNIEnv;this:JObject);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
 Log.__android_log_write(Log.ANDROID_LOG_DEBUG,'libjumpgame','Entering nativeDestroy . . .');
 GameCore.Done;
 SoundSystem.Destroy;
 Log.__android_log_write(Log.ANDROID_LOG_DEBUG,'libjumpgame','Leaving nativeDestroy . . .');
end;

procedure Java_com_bero_games_foembjump_JumpGame_nativeReinit(Env:PJNIEnv;this:JObject;Width,Height:JInt);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
 Log.__android_log_write(Log.ANDROID_LOG_DEBUG,'libjumpgame','Entering nativeReinit . . .');
 GameCore.Reinit(Width,Height);
 StartTime:=GetNowTime;
 NowTime:=GetNowTime-StartTime;
 LastTime:=NowTime;
 RenderNowTime:=GetNowTime-StartTime;
 RenderLastTime:=RenderNowTime;
 if State.State=sTITLESCREEN then begin
  PartStartTime:=NowTime;
 end;
 Log.__android_log_write(Log.ANDROID_LOG_DEBUG,'libjumpgame','Leaving nativeReinit . . .');
end;

procedure Java_com_bero_games_foembjump_JumpGame_nativeLogicStep(Env:PJNIEnv;this:JObject);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
 NowTime:=GetNowTime-StartTime;
 GameCore.Process(NowTime-LastTime);
 LastTime:=NowTime;
end;

function Java_com_bero_games_foembjump_JumpGame_nativeRenderStep(Env:PJNIEnv;this:JObject):JBoolean;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
 RenderNowTime:=GetNowTime-StartTime;
 if GameCore.Render(RenderNowTime-RenderLastTime) then begin
  RenderLastTime:=RenderNowTime;
  result:=JNI_TRUE;
 end else begin
  result:=JNI_FALSE;
 end;
end;

procedure Java_com_bero_games_foembjump_JumpGame_nativeFillBuffer(Env:PJNIEnv;this:JObject;Buffer:JObject);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
 GameCore.UpdateSound;
 SoundSystem.FillBuffer;
 Move(SoundSystem.OutputBuffer^,Env^.GetDirectBufferAddress(Env,Buffer)^,SoundSystem.OutputBufferSize);
end;

procedure Java_com_bero_games_foembjump_JumpGame_nativeResume(Env:PJNIEnv;this:JObject);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
 GameCore.Resume;
end;

procedure Java_com_bero_games_foembjump_JumpGame_nativeSuspend(Env:PJNIEnv;this:JObject);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
 GameCore.Suspend;
end;

procedure Java_com_bero_games_foembjump_JumpGame_nativeCleanUp(Env:PJNIEnv;this:JObject);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
 GameCore.CleanUp;
end;

procedure Java_com_bero_games_foembjump_JumpGame_nativeAccelerometerChange(Env:PJNIEnv;this:JObject;x,y,z:single);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
 GameCore.AccelerometerChange(x,y,z);
end;

procedure Java_com_bero_games_foembjump_JumpGame_nativeTouch(Env:PJNIEnv;this:JObject;x,y:single);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
 GameCore.Touch(x,y);
end;

function Java_com_bero_games_foembjump_JumpGame_nativeGetStateSize(Env:PJNIEnv;this:JObject):JInt;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
 result:=sizeof(TState);
end;

procedure Java_com_bero_games_foembjump_JumpGame_nativeSaveState(Env:PJNIEnv;this:JObject;Buffer:JObject);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
 Move(GameCore.State,Env^.GetDirectBufferAddress(Env,Buffer)^,sizeof(TState));
end;

procedure Java_com_bero_games_foembjump_JumpGame_nativeLoadState(Env:PJNIEnv;this:JObject;Buffer:JObject;Size:JInt);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
 if Size=sizeof(TState) then begin
  Move(Env^.GetDirectBufferAddress(Env,Buffer)^,GameCore.State,sizeof(TState));
  ReinitPointers;
 end;
end;

function Java_com_bero_games_foembjump_JumpGame_nativeBack(Env:PJNIEnv;this:JObject):JBoolean;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
 if State.State<>sTITLESCREEN then begin
  BackToTitleScreen;
  result:=JNI_TRUE;
 end else begin
  result:=JNI_FALSE;
 end;
end;

exports Java_com_bero_games_foembjump_JumpGame_nativeCreate name 'Java_com_bero_games_foembjump_JumpGame_nativeCreate',
        Java_com_bero_games_foembjump_JumpGame_nativeDestroy name 'Java_com_bero_games_foembjump_JumpGame_nativeDestroy',
        Java_com_bero_games_foembjump_JumpGame_nativeReinit name 'Java_com_bero_games_foembjump_JumpGame_nativeReinit',
        Java_com_bero_games_foembjump_JumpGame_nativeLogicStep name 'Java_com_bero_games_foembjump_JumpGame_nativeLogicStep',
        Java_com_bero_games_foembjump_JumpGame_nativeRenderStep name 'Java_com_bero_games_foembjump_JumpGame_nativeRenderStep',
        Java_com_bero_games_foembjump_JumpGame_nativeFillBuffer name 'Java_com_bero_games_foembjump_JumpGame_nativeFillBuffer',
        Java_com_bero_games_foembjump_JumpGame_nativeResume name 'Java_com_bero_games_foembjump_JumpGame_nativeResume',
        Java_com_bero_games_foembjump_JumpGame_nativeSuspend name 'Java_com_bero_games_foembjump_JumpGame_nativeSuspend',
        Java_com_bero_games_foembjump_JumpGame_nativeCleanUp name 'Java_com_bero_games_foembjump_JumpGame_nativeCleanUp',
        Java_com_bero_games_foembjump_JumpGame_nativeAccelerometerChange name 'Java_com_bero_games_foembjump_JumpGame_nativeAccelerometerChange',
        Java_com_bero_games_foembjump_JumpGame_nativeTouch name 'Java_com_bero_games_foembjump_JumpGame_nativeTouch',
        Java_com_bero_games_foembjump_JumpGame_nativeGetStateSize name 'Java_com_bero_games_foembjump_JumpGame_nativeGetStateSize',
        Java_com_bero_games_foembjump_JumpGame_nativeSaveState name 'Java_com_bero_games_foembjump_JumpGame_nativeSaveState',
        Java_com_bero_games_foembjump_JumpGame_nativeLoadState name 'Java_com_bero_games_foembjump_JumpGame_nativeLoadState',
        Java_com_bero_games_foembjump_JumpGame_nativeBack name 'Java_com_bero_games_foembjump_JumpGame_nativeBack';

begin
end.
{$else}

type TLogicThread=class(TThread)
      protected
       procedure Execute; override;
     end;

{    TRenderThread=class(TThread)
      protected
       procedure Execute; override;
     end;}

var Event:TSDL_Event;
    Surface:PSDL_Surface;
    VideoInfo:PSDL_VideoInfo;
    VideoFlags:longword;
    SDLWaveFormat:TSDL_AudioSpec;
    BufPosition:integer;
    LogicCriticalSection:TBeRoCriticalSection;
//  RenderCriticalSection:TBeRoCriticalSection;
    LogicThread:TLogicThread;
//  RenderThread:TRenderThread;

procedure TerminateApplication;
begin
 SDL_QUIT;
 Halt(0);
end;

procedure SDLFillBuffer(UserData:pointer;Stream:puint8;Remain:integer); cdecl;
var Len,BufPos:integer;
begin
 try
  BufPos:=0;
  while Remain>0 do begin
   if BufPosition>=SoundSystem.OutputBufferSize then begin
    BufPosition:=0;
    GameCore.UpdateSound;
    SoundSystem.FillBuffer;
   end;
   Len:=SoundSystem.OutputBufferSize-BufPosition;
   if Len>Remain then begin
    Len:=Remain;
   end;
   move(pansichar(SoundSystem.OutputBuffer)[BufPosition],pansichar(Stream)[BufPos],Len);
   inc(BufPosition,Len);
   inc(BufPos,Len);
   dec(Remain,Len);
  end;
 except
 end;
end;

procedure TLogicThread.Execute;
begin
 while not Terminated do begin
  LogicCriticalSection.Enter;
  try
   NowTime:=SDL_GetTicks-StartTime;
   GameCore.Process(NowTime-LastTime);
   LastTime:=NowTime;
  finally
   LogicCriticalSection.Leave;
  end;
  sleep(10);
 end;
end;

{procedure TRenderThread.Execute;
var RenderNowTime,RenderLastTime:int64;
begin
 RenderNowTime:=SDL_GetTicks;
 RenderLastTime:=RenderNowTime;
 while not Terminated do begin
  RenderCriticalSection.Enter;
  try
   RenderNowTime:=SDL_GetTicks;
   if GameCore.Render(RenderNowTime-RenderLastTime) then begin
    RenderLastTime:=RenderNowTime;
    SDL_GL_SwapBuffers;
   end;
  finally
   RenderCriticalSection.Leave;
  end;
  sleep(10);
 end;
end; }

var i,j:longword;
    SDLRunning,ShowCursor,OldShowCursor:boolean;
    k,RenderNowTime,RenderLastTime:int64;   
begin
 Log.LogStatus('Initializing data...','Main');
 InitializeData;

 ShowCursor:=true;
 OldShowCursor:=false;

 SCREEN_WIDTH:=StrToIntDef(paramstr(1),480);
 SCREEN_HEIGHT:=StrToIntDef(paramstr(2),800);

//SDL_putenv('__GL_SYNC_TO_VBLANK=0');
 if SDL_Init(SDL_INIT_VIDEO or SDL_INIT_TIMER or SDL_INIT_NOPARACHUTE or SDL_INIT_JOYSTICK {$ifndef linux}or SDL_INIT_AUDIO{$endif})<0 then begin
  Log.LogError('Could not initialize SDL : '+SDL_GetError,'Main');
  TerminateApplication;
 end;
 VideoInfo:=SDL_GetVideoInfo;
 if not assigned(VideoInfo) then begin
  Log.LogError('Video query failed : '+SDL_GetError,'Main');
  TerminateApplication;
 end;
 VideoFlags:=SDL_DOUBLEBUF or SDL_HWPALETTE or SDL_RESIZABLE or SDL_OPENGL;
 if VideoInfo^.hw_available<>0 then begin
  VideoFlags:=VideoFlags or SDL_HWSURFACE;
 end else begin
  VideoFlags:=VideoFlags or SDL_SWSURFACE;
 end;
 if VideoInfo^.blit_hw<>0 then begin
  VideoFlags:=VideoFlags or SDL_HWACCEL;
 end;
 if SCREEN_WIDTH>=((VideoInfo^.current_W*90) div 100) then begin
  k:=((VideoInfo^.current_W*90) div 100);
  SCREEN_HEIGHT:=(SCREEN_HEIGHT*k) div SCREEN_WIDTH;
  SCREEN_WIDTH:=k;
 end;
 if SCREEN_HEIGHT>=((VideoInfo^.current_H*90) div 100) then begin
  k:=((VideoInfo^.current_H*90) div 100);
  SCREEN_WIDTH:=(SCREEN_WIDTH*k) div SCREEN_HEIGHT;
  SCREEN_HEIGHT:=k;
 end;
 Surface:=SDL_SetVideoMode(SCREEN_WIDTH,SCREEN_HEIGHT,SCREEN_BPP,VideoFlags);
 if not assigned(Surface) then begin
  Log.LogError('Unable to create screen : '+SDL_GetError,'Main');
  TerminateApplication;
 end;
 SDL_ShowCursor(1);
 SDLRunning:=true;

 Log.LogStatus('Initializing OpenGL...','Main');
 if Load_GL_version_1_2 and Load_GL_version_1_3 and Load_GL_version_1_4 and Load_GL_version_1_5 and Load_GL_version_2_0 then begin

  if SDL_WasInit(SDL_INIT_AUDIO)=0 then begin
   if SDL_Init(SDL_INIT_AUDIO)<0 then begin
    Log.LogError('Unable to initialize SDL audio : '+SDL_GetError,'Main');
    TerminateApplication;
   end;
  end;
  FillChar(SDLWaveFormat,SIZEOF(TSDL_AudioSpec),#0);
  SDLWaveFormat.Channels:=2;
  SDLWaveFormat.Format:=AUDIO_S16;
  SDLWaveFormat.Freq:=44100;
  SDLWaveFormat.Callback:=@SDLFillBuffer;
  SDLWaveFormat.silence:=0;
  SDLWaveFormat.Samples:=1024;
  SDLWaveFormat.Size:=SDLWaveFormat.Samples*2*2;
  SDLWaveFormat.UserData:=pointer(SoundSystem);
  SoundSystem:=TBeRoSoundSystem.Create(44100,2,16,SDLWaveFormat.Samples);
  BufPosition:=SoundSystem.OutputBufferSize;

  Log.LogStatus('Initializing audio...','Main');
  SDL_OpenAudio(@SDLWaveFormat,nil);
  SDL_PauseAudio(1);
  StartTime:=SDL_GetTicks;
  LastTime:=SDL_GetTicks-StartTime;

  SDL_EventState(SDL_MOUSEMOTION,SDL_ENABLE);
  SDL_EventState(SDL_MOUSEBUTTONDOWN,SDL_ENABLE);
  SDL_EventState(SDL_KEYDOWN,SDL_ENABLE);
  SDL_EventState(SDL_QUITEV,SDL_ENABLE);
  SDL_EventState(SDL_VIDEORESIZE,SDL_ENABLE);

  LogicCriticalSection:=TBeRoCriticalSection.Create;
//RenderCriticalSection:=TBeRoCriticalSection.Create;

  GameCore.Init;
  GameCore.Reinit(SCREEN_WIDTH,SCREEN_HEIGHT);

  SDL_PauseAudio(0);

  LogicThread:=TLogicThread.Create(false);
//RenderThread:=TRenderThread.Create(false);

  RenderNowTime:=SDL_GetTicks;
  RenderLastTime:=RenderNowTime;

  SDL_EventState(SDL_MOUSEMOTION,SDL_IGNORE);
  SDL_WarpMouse(SCREEN_WIDTH shr 1,SCREEN_HEIGHT shr 1);
  SDL_EventState(SDL_MOUSEMOTION,SDL_ENABLE);

  SDL_WM_SetCaption('FoembJump',nil);

  Log.LogStatus('Entering main loop...','Main');
  while SDLRunning do begin
   i:=SDL_GetTicks;
   j:=0;
   while ((j=0) or (abs(SDL_GetTicks-i)<20)) and (SDL_PollEvent(@Event)<>0) do begin
    inc(j);
    case Event.type_ of
     SDL_QUITEV:begin
      SDLRunning:=false;
      break;
     end;
     SDL_KEYDOWN:begin
      case Event.key.keysym.sym of
       SDLK_ESCAPE:begin
        if State.State=sTITLESCREEN then begin
         SDLRunning:=false;   
         break;
        end else begin
         BackToTitleScreen;
        end;
       end;
       SDLK_F4:begin
        if (Event.key.keysym.modifier and ((KMOD_LALT or KMOD_RALT) or (KMOD_LMETA or KMOD_RMETA)))<>0 then begin
         SDLRunning:=false;
         break;
        end;
       end;
      end;
     end;
     SDL_KEYUP:begin
     end;
     SDL_VIDEORESIZE:begin
      LogicCriticalSection.Enter;
      try
{      RenderCriticalSection.Enter;
       try}
        if assigned(Surface) then begin
         SDL_FreeSurface(Surface);
         Surface:=nil;
        end;
        SCREEN_WIDTH:=Event.resize.w;
        SCREEN_HEIGHT:=Event.resize.h;
        Surface:=SDL_SetVideoMode(SCREEN_WIDTH,SCREEN_HEIGHT,SCREEN_BPP,VideoFlags);
        if not assigned(Surface) then begin
         Log.LogError('Unable to create screen : '+SDL_GetError,'Main');
         TerminateApplication;
        end;
        GameCore.Reinit(SCREEN_WIDTH,SCREEN_HEIGHT);
{      finally
        RenderCriticalSection.Leave;
       end;}
      finally
       LogicCriticalSection.Leave;
      end;
     end;
     SDL_MOUSEMOTION:begin
      iF State.State in [sPRESTARTSCREEN,sGAME] then begin
      //GameCore.State.Player.x:=GameCore.State.Player.x+(Event.motion.xrel*(CANVAS_WIDTH/SCREEN_WIDTH));
       LogicCriticalSection.Enter;
       try
        GameCore.AccelerometerChange(Event.motion.xrel/(SCREEN_WIDTH*0.5),Event.motion.yrel/(SCREEN_HEIGHT*0.5),0.0);
       finally
        LogicCriticalSection.Leave;
       end;
       SDL_EventState(SDL_MOUSEMOTION,SDL_IGNORE);
       SDL_WarpMouse(SCREEN_WIDTH shr 1,SCREEN_HEIGHT shr 1);
       SDL_EventState(SDL_MOUSEMOTION,SDL_ENABLE);
      end;
     end;
     SDL_MOUSEBUTTONDOWN:begin
      if State.State in [sTITLESCREEN] then begin
       SDL_EventState(SDL_MOUSEMOTION,SDL_IGNORE);
       SDL_WarpMouse(SCREEN_WIDTH shr 1,SCREEN_HEIGHT shr 1);
       SDL_EventState(SDL_MOUSEMOTION,SDL_ENABLE);
      end;
      GameCore.Touch(Event.button.x,Event.button.y);
     end;
    end;
   end;
   if SDLRunning then begin
    ShowCursor:=State.State=sTITLESCREEN;
    if ShowCursor<>OldShowCursor then begin
     OldShowCursor:=ShowCursor;  
     if ShowCursor then begin
      SDL_ShowCursor(1);
     end else begin
      SDL_EventState(SDL_MOUSEMOTION,SDL_IGNORE);
      SDL_WarpMouse(SCREEN_WIDTH shr 1,SCREEN_HEIGHT shr 1);
      SDL_EventState(SDL_MOUSEMOTION,SDL_ENABLE);
      SDL_ShowCursor(0);
     end; 
    end;
{   SDL_EventState(SDL_MOUSEMOTION,SDL_IGNORE);
    SDL_WarpMouse(SCREEN_WIDTH shr 1,SCREEN_HEIGHT shr 1);
    SDL_EventState(SDL_MOUSEMOTION,SDL_ENABLE);}
    RenderNowTime:=SDL_GetTicks;
    if GameCore.Render(RenderNowTime-RenderLastTime) then begin
     RenderLastTime:=RenderNowTime;
     SDL_GL_SwapBuffers;
     sleep(0);
    end;
   end;
  end;
  Log.LogStatus('Main loop leaved...','Main');
  LogicThread.Terminate;
//RenderThread.Terminate;
  LogicThread.WaitFor;
//RenderThread.WaitFor;
  LogicThread.Destroy;
//RenderThread.Destroy;
  SDL_PauseAudio(1);
  SDL_CloseAudio;
  GameCore.Done;
  SoundSystem.Destroy;
  LogicCriticalSection.Destroy;
//RenderCriticalSection.Destroy;
 end else begin
  Log.LogError('Unable to load needed OpenGL extensions : '+SDL_GetError,'Main');
 end;
 if assigned(Surface) then begin
  SDL_FreeSurface(Surface);
  Surface:=nil;
 end;
 TerminateApplication;
end.
{$endif}

