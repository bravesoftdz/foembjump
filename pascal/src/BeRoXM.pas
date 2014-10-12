////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                                   BeRoXM                                   //
//               Copyright (C) 2005-2010, Benjamin 'BeRo' Rosseaux            //
//                                                                            //
////////////////////////////// Disclaimer and legal ////////////////////////////
//                                                                            //
// - BeRoXM Sourcecode is copyright (C) 2005-201o, Benjamin 'BeRo' Rosseaux   //
// - This source must not be redistributed without this text.                 //
// - This source is provided as-is. BeRo (Benjamin Rosseaux) will not support //
//   or answer questions about the source provided. unless there are some     //
//   REALLY nasty bugs. Remember i'm NOT going to spend time on this, except  //
//   if you are using it in a commerical product! Otherwhise fix it yourself! //
// - The source can be modified, and redistributed as long as no copyright or //
//   comment blocks (see at the top of each source file) are removed.         //
// - BeRoXM sourcecode is in no way representative of BeRoTracker source.     //
// - BeRoXM may not replay XM files 100% as BeRoTracker does.                 //
// - If your product is not intended to make any money, and is not charged    //
//   for in any way, then you may use this in it for FREE!                    //
// - You may use it for free until your commercial product is about to be     //
//   published.                                                               //
// - If you want to use it in a commercial product, then please contact me    //
//   (benjamin@0ok.de) for pricing and conditions.                            //
// - You must give credits!                                                   //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Designed for small graphic demos as small music playback engine            //
//                                                                            //
// And don't forget to write my name in your credits of your demo or app ;)   //
//                                                                            //
// http://www.bero.0ok.de/ - http://www.0ok.de/ - http://www.berotracker.de/  //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
unit BeRoXM;
{$define BeRoXMRingBuffer}
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
 {$define UseSAR}
{$endif}
{$overflowchecks off}
{$rangechecks off}
{$ifndef MixerOwnDefine}
 {$undef MixerFloatingPointIncrementCalculation}
 {$define UnrolledLoops}
 {$define MixerNearest}
 {$define MixerLinear}
 {$ifdef cpuarm}
  {$undef MixerCubicSpline}
  {$undef MixerWindowedFIR}
 {$else}
  {$define MixerCubicSpline}
  {$define MixerWindowedFIR}
 {$endif}
 {$define UseMIDIMacros}
 {$define UseAmigaMODLoader}
 {$define UseDPCM4}
 {$define UseFilters}
 {$define UseMono}
 {$define UseStereo}
 {$define UseSaver}
{$endif}
{$ifdef cpuarm}
 {$define MixerNoINT64}
 {$define UseSAR}
{$endif}
{$ifdef MixerNoINT64}
 {$undef MixerIntegerIncrement}
{$endif}
{$ifndef UseNoTables}
 {$define UseTables}
 {$define UseFilterCoefTables}
{$endif}
{$undef windows}
{$ifdef win32}
 {$define windows}
{$endif}
{$ifdef win64}
 {$define windows}
{$endif}
{$ifdef wince}
 {$define windows}
{$endif}
{$ifndef windows}
 {$undef UseDirectSound}
{$endif}

interface

{$ifdef wince}
const MAXPNAMELEN=32;

      TIME_MS=$0001;
      TIME_SAMPLES=$0002;
      TIME_BYTES=$0004;
      TIME_SMPTE=$0008;
      TIME_MIDI=$0010;
      TIME_TICKS=$0020;

type dword=longword;
     PWAVEHDR=^WAVEHDR;
     WAVEHDR=packed record
      lpData:pchar;
      dwBufferLength:dword;
      dwBytesRecorded:dword;
      dwUser:dword;
      dwFlags:dword;
      dwLoops:dword;
      lpNext:PWAVEHDR;
      reserved:dword;
     end;

     PWAVEFORMATEX=^WAVEFORMATEX;
     WAVEFORMATEX=packed record
      wFormatTag:word;
      nChannels:word;
      nSamplesPerSec:dword;
      nAvgBytesPerSec:dword;
      nBlockAlign:word;
      wBitsPerSample:word;
      cbSize:word;
     end;
     TWAVEFORMATEX=WAVEFORMATEX;

     PWAVEOUTCAPSW=^TWAVEOUTCAPSW;
     TWAVEOUTCAPSW=packed record
      wMid:Word;                 { manufacturer ID }
      wPid:Word;                 { product ID }
      vDriverVersion:MMVERSION;       { version of the driver }
      szPname:array[0..MAXPNAMELEN-1] of WideChar;  { product name (NULL terminated string) }
      dwFormats:DWORD;          { formats supported }
      wChannels:Word;            { number of sources supported }
      dwSupport:DWORD;          { functionality supported by driver }
     end;

     PMMTime=^TMMTime;
     TMMTime=packed record
      case wType:UINT of
       TIME_MS:(ms:DWORD);
       TIME_SAMPLES:(sample:DWORD);
       TIME_BYTES:(cb:DWORD);
       TIME_TICKS:(ticks:DWORD);
       TIME_SMPTE:(
        hour:Byte;
        min:Byte;
        sec:Byte;
        frame:Byte;
        fps:Byte;
        dummy:Byte;
        pad:array[0..1] of Byte);
       TIME_MIDI:(songptrpos:DWORD);
     end;

     TCRITICAL_SECTION=TRTLCriticalSection;
     PCRITICAL_SECTION=PRTLCriticalSection;
     TCRITICALSECTION=TRTLCriticalSection;
     PCRITICALSECTION=PRTLCriticalSection;

     UINT=dword;
     MMRESULT=UINT;
     MMVERSION=UINT;
     HWAVEOUT=THandle;
     LPHWAVEOUT=^HWAVEOUT;
     HWAVEIN=THandle;
     LPHWAVEIN=^HWAVEOUT;
     HWAVE=THandle;
     LPHWAVE=^THandle;
     LPUINT=^UINT;
     HANDLE=UINT;

     TFarProc=pointer;

     TFNThreadStartRoutine=TFarProc;
     TFNFiberStartRoutine=TFarProc;

     HINST=longword;
     HRSRC=longword;
     HGLOBAL=THandle;

     MakeIntResource=pchar;

const CoreDll='coredll.dll';
      WAVE_FORMAT_PCM=1;
      WAVE_MAPPER=dword(-1);
      WHDR_DONE=1;
      WHDR_PREPARED=2;
      WHDR_BEGINLOOP=$4;
      WHDR_ENDLOOP=$8;
      WHDR_INQUEUE=$10;
      WAVERR_BASE=32;
      WAVERR_BADFORMAT=WAVERR_BASE+0;
      WAVERR_STILLPLAYING=WAVERR_BASE+1;
      WAVERR_UNPREPARED=WAVERR_BASE+2;
      WAVERR_SYNC=WAVERR_BASE+3;
      WAVERR_LASTERROR=WAVERR_BASE+3;
      CREATE_SUSPENDED=4;

      THREAD_PRIORITY_TIME_CRITICAL=0;
      THREAD_PRIORITY_HIGHEST=1;
      THREAD_PRIORITY_ABOVE_NORMAL=2;
      THREAD_PRIORITY_NORMAL=3;
      THREAD_PRIORITY_BELOW_NORMAL=4;
      THREAD_PRIORITY_LOWEST=5;
      THREAD_PRIORITY_ABOVE_IDLE=6;
      THREAD_PRIORITY_IDLE=7;

      RT_RCDATA=MakeIntResource(10);

      MMSYSERR_NOERROR=0;

procedure sleep(dwMilliseconds:dword); stdcall; external CoreDll name 'sleep';
function waveOutOpen(x1: LPHWAVEOUT; x2: UINT; x3: PWAVEFORMATEX; x4: dword; x5: dword; x6: dword): MMRESULT; stdcall; external CoreDll name 'waveOutOpen';
function waveOutClose(x1: HWAVEOUT): MMRESULT; stdcall; external CoreDll name 'waveOutClose';
function waveOutPrepareHeader(x1: HWAVEOUT; x2: PWAVEHDR; x3: UINT): MMRESULT; stdcall; external CoreDll name 'waveOutPrepareHeader';
function waveOutUnprepareHeader(x1: HWAVEOUT; x2: PWAVEHDR; x3: UINT): MMRESULT; stdcall; external CoreDll name 'waveOutUnprepareHeader';
function waveOutWrite(x1: HWAVEOUT; x2: PWAVEHDR; x3: UINT): MMRESULT; stdcall; external CoreDll name 'waveOutWrite';
function waveOutReset(x1: HWAVEOUT): MMRESULT; stdcall; external CoreDll name 'waveOutReset';
function waveOutGetDevCapsW(uDeviceID: UINT; lpCaps: PWAVEOUTCAPSW; uSize: UINT): MMRESULT; stdcall; external CoreDll name 'waveOutGetDevCapsW';
function waveOutGetPosition(hWaveOut: HWAVEOUT; lpInfo: PMMTime; uSize: UINT): MMRESULT; stdcall; external CoreDll name 'waveOutGetPosition';
function timeGetTime: DWORD; stdcall; external CoreDll name 'timeGetTime';
//function SetThreadPriority(hThread:HANDLE; nPriority:longint):LONGBOOL; stdcall; external CoreDll name 'SetThreadPriority';
//function TerminateThread(hThread:HANDLE; dwExitCode:DWORD):LONGBOOL; stdcall; external CoreDll name 'TerminateThread';
function CreateEvent(lpEventAttributes: pointer; bManualReset, bInitialState: LONGBOOL; lpName: PWideChar): HANDLE; stdcall; external CoreDll name 'CreateEventW';
function WaitForSingleObject(hHandle:HANDLE; dwMilliseconds:DWORD):DWORD; stdcall; external CoreDll name 'WaitForSingleObject';
function SetEvent(hEvent: HANDLE): LONGBOOL; stdcall; external CoreDll name 'SetEvent';
function CloseHandle(hObject:HANDLE):LONGBOOL; stdcall; external CoreDll name 'CloseHandle';
//function CreateThread(lpThreadAttributes: Pointer; dwStackSize: DWORD; lpStartAddress: TFNThreadStartRoutine; lpParameter: Pointer; dwCreationFlags: DWORD; var lpThreadId: DWORD): THandle; stdcall; external CoreDll name 'CreateThread';
{function FindResource(hModule: HMODULE; lpName, lpType: PChar): HRSRC; stdcall; external CoreDll name 'FindResource';
function LoadResource(hModule: HINST; hResInfo: HRSRC): HGLOBAL; stdcall; external CoreDll name 'LoadResource';
function SizeofResource(hModule: HINST; hResInfo: HRSRC): DWORD; stdcall; external CoreDll name 'SizeofResource';
function LockResource(hResData: HGLOBAL): Pointer; stdcall; external CoreDll name 'LockResource';}
function MessageBox(w1:longint;l1,l2:pwidechar;w2:longint):longint; stdcall;external coredll name 'MessageBoxW';
{$else}
{$ifdef unix}
uses {$ifndef android}cthreads,{$endif}SysUtils{$ifndef android},BaseUnix,Unix{$endif}{$ifdef sdl},sdl{$endif};

{$ifndef android}
      // Supported card ID numbers (OBSOLETE. NOT USED ANY MORE)

const SNDCARD_ADLIB=1;
      SNDCARD_SB=2;
      SNDCARD_PAS=3;
      SNDCARD_GUS=4;
      SNDCARD_MPU401=5;
      SNDCARD_SB16=6;
      SNDCARD_SB16MIDI=7;
      SNDCARD_UART6850=8;
      SNDCARD_GUS16=9;
      SNDCARD_MSS=10;
      SNDCARD_PSS=11;
      SNDCARD_SSCAPE=12;
      SNDCARD_PSS_MPU=13;
      SNDCARD_PSS_MSS=14;
      SNDCARD_SSCAPE_MSS=15;
      SNDCARD_TRXPRO=16;
      SNDCARD_TRXPRO_SB=17;
      SNDCARD_TRXPRO_MPU=18;
      SNDCARD_MAD16=19;
      SNDCARD_MAD16_MPU=20;
      SNDCARD_CS4232=21;
      SNDCARD_CS4232_MPU=22;
      SNDCARD_MAUI=23;
      SNDCARD_PSEUDO_MSS=24;
      SNDCARD_GUSPNP=25;
      SNDCARD_UART401=26;
      // Sound card numbers 27 to N are reserved. Don't add more numbers here

      // IOCTL commands for /dev/dsp and /dev/audio
      SNDCTL_DSP_RESET=$5000;
      SNDCTL_DSP_SPEED=$c0045002;
      SNDCTL_DSP_STEREO=$c0045003;
      SNDCTL_DSP_GETBLKSIZE=$c0045004;
      SNDCTL_DSP_SAMPLESIZE=$c0045005;
      SNDCTL_DSP_CHANNELS=$c0045006;
      SOUND_PCM_WRITE_CHANNELS=$c0045006;
      SOUND_PCM_WRITE_FILTER=$c0045007;
      SNDCTL_DSP_POST=$5008;
      SNDCTL_DSP_SUBDIVIDE=$c0045009;
      SNDCTL_DSP_SETFRAGMENT=$c004500a;

      // Audio data formats (Note! U8=8 and S16_LE=16 for compatibility)*/ }

      SNDCTL_DSP_GETFMTS=$8004500b;
      SNDCTL_DSP_SETFMT=$c0045005;

      AFMT_QUERY=0;
      AFMT_MU_LAW=1;
      AFMT_A_LAW =2;
      AFMT_IMA_DPCM=4;
      AFMT_U8=8;
      AFMT_S16_LE=$10;
      AFMT_S16_BE=$20;
      AFMT_S8=$40;
      AFMT_U16_LE=$80;
      AFMT_U16_BE=$100;
      AFMT_MPEG=$200;

      // 32 bit formats (MSB aligned) formats

      AFMT_S32_LE=$1000;
      AFMT_S32_BE=$2000;

      // AC3 _compressed_ bitstreams (See Programmer's Guide for details).

      AFMT_AC3=$4000;

      // 24 bit formats (LSB aligned in 32 bit word) formats*/ }
      AFMT_S24_LE=$8000;
      AFMT_S24_BE =$10000;

      { S/PDIF raw format. In this format the S/PDIF frames (including all
       control and user bits) are included in the data stream. Each sample
       is stored in a 32 bit frame (see IEC-958 for more info). This format
       is supported by very few devices and it's only usable for purposes
       where full access to the control/user bits is required (real time
       control). }

      AFMT_SPDIF_RAW=$20000;

      SNDCTL_DSP_GETOSPACE=$8010500c;
      SNDCTL_DSP_GETISPACE=$8010500d;
      SNDCTL_DSP_NONBLOCK=$500e;
      SNDCTL_DSP_GETCAPS=$8004500f;

      DSP_CAP_REVISION=$ff;  // Bits for revision level (0 to 255)
      DSP_CAP_DUPLEX=$100;   // Full duplex record/playback
      DSP_CAP_REALTIME=$200; // Not in use
      DSP_CAP_BATCH=$400;    (* Device has some kind of
                                internal buffers which may
                                cause some delays and
                                decrease precision of timing *)

      DSP_CAP_COPROC=$800;  // Has a coprocessor
      (* Sometimes it's a DSP
         but usually not *)

      DSP_CAP_TRIGGER=$1000;  // Supports SETTRIGGER
      DSP_CAP_MMAP=$2000;     // Supports mmap()
      DSP_CAP_MULTI=$4000;    // Supports multiple open
      DSP_CAP_BIND=$8000;     // Supports binding to front/rear/center/lfe
      DSP_CAP_INPUT=$10000;   // Supports recording
      DSP_CAP_OUTPUT=$20000;  // Supports playback
      DSP_CAP_VIRTUAL=$40000; // Virtuial device
      // Analog/digital control capabilities
      DSP_CAP_ANALOGOUT=$100000;
      DSP_CAP_ANALOGIN=$200000;
      DSP_CAP_DIGITALOUT=$400000;
      DSP_CAP_DIGITALIN=$800000;
      DSP_CAP_ADMASK=$f00000;
      { NOTE! (capabilities and DSP_CAP_ADMASK)=0 means just that the
       digital/analog interface control features are not supported by the
       device/driver. However the device still supports analog, digital or
       both inputs/outputs (depending on the device). See the OSS Programmer's
       Guide for full details. }

      SNDCTL_DSP_GETTRIGGER=$80045010;
      SNDCTL_DSP_SETTRIGGER=$40045010;

      PCM_ENABLE_INPUT=1;
      PCM_ENABLE_OUTPUT=2;

      SNDCTL_DSP_GETIPTR=$800c5011;
      SNDCTL_DSP_GETOPTR=$800c5012;

      SNDCTL_DSP_MAPINBUF=$80085013;
      SNDCTL_DSP_MAPOUTBUF=$80085014;
      SNDCTL_DSP_SETSYNCRO=$5015;
      SNDCTL_DSP_SETDUPLEX=$5016;

      // Application's profile defines the way how playback underrun situations should be handled.

      { APF_NORMAL (the default) and APF_NETWORK make the driver to cleanup the
        playback buffer whenever an underrun occurs. This consumes some time
        prevents looping the existing buffer.
        APF_CPUINTENS is intended to be set by CPU intensive applications which
        are likely to run out of time occasionally. In this mode the buffer
        cleanup is disabled which saves CPU time but also let's the previous
        buffer content to be played during the "pause" after the underrun. }

      SNDCTL_DSP_PROFILE=$40045017;

      APF_NORMAL=0;   // Normal applications
      APF_NETWORK=1; //  Underruns probably caused by an 'external' delay
      APF_CPUINTENS=2; // Underruns probably caused by 'overheating' the CPU

      SNDCTL_DSP_GETODELAY=$80045017;

      DIG_CBITIN_LIMITED=$00000001;
      DIG_CBITIN_DATA=$00000002;
      DIG_CBITIN_BYTE0=$00000004;
      DIG_CBITIN_FULL=$00000008;
      DIG_CBITIN_MASK=$0000000;
      DIG_CBITOUT_LIMITED=$00000010;
      DIG_CBITOUT_BYTE0=$00000020;
      DIG_CBITOUT_FULL=$00000040;
      DIG_CBITOUT_DATA=$00000080;
      DIG_CBITOUT_MASK=$000000f0;
      DIG_UBITIN=$00000100;
      DIG_UBITOUT=$00000200;

      VAL_CBITIN=$01;
      VAL_UBITIN=$02;
      VAL_CBITOUT=$04;
      VAL_UBITOUT=$08;
      VAL_ISTATUS=$10;

      OUTSEL_DIGITAL=1;
      OUTSEL_ANALOG=2;
      OUTSEL_BOTH=(OUTSEL_DIGITAL or OUTSEL_ANALOG);

      IND_UNKNOWN=0;
      IND_AUDIO=1;
      IND_DATA=2;

      LOCK_NOT_INDICATED=0;
      LOCK_UNLOCKED=1;
      LOCK_LOCKED=2;
      IN_QUAL_NOT_INDICATED=0;
      IN_QUAL_POOR=1;
      IN_QUAL_GOOD=2;

      VBIT_NOT_INDICATED=0;
      VBIT_OFF=1;
      VBIT_ON=2;

      INERR_CRC=$0001;
      INERR_QCODE_CRC=$0002;
      INERR_PARITY=$0004;
      INERR_BIPHASE=$0008;

      SNDCTL_DSP_GETCHANNELMASK=$c0045040;
      SNDCTL_DSP_BIND_CHANNEL=$c0045041;

      DSP_BIND_QUERY=0;
      DSP_BIND_FRONT=1;
      DSP_BIND_SURR=2;
      DSP_BIND_CENTER_LFE=4;
      DSP_BIND_HANDSET=8;
      DSP_BIND_MIC=$10;
      DSP_BIND_MODEM1=$20;
      DSP_BIND_MODEM2=$40;
      DSP_BIND_I2S=$80;
      DSP_BIND_SPDIF=$100;

      //  Mixer devices

      { There can be up to 20 different analog mixer channels. The
         SOUND_MIXER_NRDEVICES gives the currently supported maximum.
         The SOUND_MIXER_READ_DEVMASK returns a bitmask which tells
         the devices supported by the particular mixer. }

      SOUND_MIXER_NRDEVICES=28;
      SOUND_MIXER_VOLUME=0;
      SOUND_MIXER_BASS=1;
      SOUND_MIXER_TREBLE=2;
      SOUND_MIXER_SYNTH=3;
      SOUND_MIXER_PCM=4;
      SOUND_MIXER_SPEAKER=5;
      SOUND_MIXER_LINE=6;
      SOUND_MIXER_MIC=7;
      SOUND_MIXER_CD=8;
      SOUND_MIXER_IMIX=9;     // Recording monitor
      SOUND_MIXER_ALTPCM=10;
      SOUND_MIXER_RECLEV=11;  // Recording level
      SOUND_MIXER_IGAIN=12;   // Input gain
      SOUND_MIXER_OGAIN=13;   // Output gain

      { The AD1848 codec and compatibles have three line level inputs
       (line, aux1 and aux2). Since each card manufacturer have assigned
       different meanings to these inputs, it's inpractical to assign
       specific meanings (line, cd, synth etc.) to them. }

      SOUND_MIXER_LINE1=14;     // Input source 1 (aux1)
      SOUND_MIXER_LINE2=15;     // Input source 2 (aux2)
      SOUND_MIXER_LINE3=16;     // Input source 3 (line)
      SOUND_MIXER_DIGITAL1=17;  // Digital (input)
      SOUND_MIXER_DIGITAL2=18;  // Digital (input) 2
      SOUND_MIXER_DIGITAL3=19;  // Digital (input) 3
      SOUND_MIXER_PHONEIN=20;   // Phone input
      SOUND_MIXER_PHONEOUT=21;  // Phone output
      SOUND_MIXER_VIDEO=22;     // Video/TV (audio) in
      SOUND_MIXER_RADIO=23;     // Radio in
      SOUND_MIXER_MONITOR=24;   // Monitor (usually mic) volume
      SOUND_MIXER_DEPTH=25;     // 3D 'depth'/'space' parameter
      SOUND_MIXER_CENTER=26;    // 3D 'center' parameter
      SOUND_MIXER_MIDI=27;      // Alternative for 'synth'

      { Some on/off settings (SOUND_SPECIAL_MIN - SOUND_SPECIAL_MAX)
        Not counted to SOUND_MIXER_NRDEVICES, but use the same number space }

      SOUND_ONOFF_MIN=28;
      SOUND_ONOFF_MAX=30;

      // Note! Number 31 cannot be used since the sign bit is reserved

      SOUND_MIXER_NONE=31;

      { The following unsupported macros are no longer functional.
        Use SOUND_MIXER_PRIVATE# macros in future. }

      SOUND_MIXER_ENHANCE=SOUND_MIXER_NONE;
      SOUND_MIXER_MUTE=SOUND_MIXER_NONE;
      SOUND_MIXER_LOUD=SOUND_MIXER_NONE;

      SOUND_DEVICE_LABELS:array[0..27] of pchar=('Vol ',
                                                 'Bass ',
                                                 'Trebl',
                                                 'Synth',
                                                 'Pcm ',
                                                 'Spkr ',
                                                 'Line ',
                                                 'Mic  ',
                                                 'CD   ',
                                                 'Mix  ',
                                                 'Pcm2 ',
                                                 'Rec  ',
                                                 'IGain',
                                                 'OGain',
                                                 'Line1',
                                                 'Line2',
                                                 'Line3',
                                                 'Digital1',
                                                 'Digital2',
                                                 'Digital3',
                                                 'PhoneIn',
                                                 'PhoneOut',
                                                 'Video',
                                                 'Radio',
                                                 'Monitor',
                                                 'Depth',
                                                 'Center',
                                                 'MIDI');


      SOUND_DEVICE_NAMES:array[0..27] of pchar=('vol',
                                                'bass',
                                                'treble',
                                                'synth',
                                                'pcm',
                                                'speaker',
                                                'line',
                                                'mic',
                                                'cd',
                                                'mix',
                                                'pcm2',
                                                'rec',
                                                'igain',
                                                'ogain',
                                                'line1',
                                                'line2',
                                                'line3',
                                                'dig1',
                                                'dig2',
                                                'dig3',
                                                'phin',
                                                'phout',
                                                'video',
                                                'radio',
                                                'monitor',
                                                'depth',
                                                'center',
                                                'midi');

      // Device bitmask identifiers

      SOUND_MIXER_RECSRC=$ff;   // Arg contains a bit for each recording source
      SOUND_MIXER_DEVMASK=$fe;  // Arg contains a bit for each supported device
      SOUND_MIXER_RECMASK=$fd;  // Arg contains a bit for each supported recording source
      SOUND_MIXER_CAPS=$fc;
      SOUND_CAP_EXCL_INPUT=$1;   // Only one recording source at a time

      SOUND_MIXER_STEREODEVS=$fb;  // Mixer channels supporting stereo

      // OSS/Free ONLY
      SOUND_MIXER_OUTSRC=$fa;   // Arg contains a bit for each input source to output
      SOUND_MIXER_OUTMASK=$f9;  // Arg contains a bit for each supported input source to output
      // OSS/Free ONLY

      // Device mask bits

      SOUND_MASK_VOLUME=1 shl SOUND_MIXER_VOLUME;
      SOUND_MASK_BASS=1 shl SOUND_MIXER_BASS;
      SOUND_MASK_TREBLE=1 shl SOUND_MIXER_TREBLE;
      SOUND_MASK_SYNTH= 1 shl SOUND_MIXER_SYNTH;
      SOUND_MASK_PCM=1 shl SOUND_MIXER_PCM;
      SOUND_MASK_SPEAKER=1 shl SOUND_MIXER_SPEAKER;
      SOUND_MASK_LINE=1 shl SOUND_MIXER_LINE;
      SOUND_MASK_MIC=1 shl SOUND_MIXER_MIC;
      SOUND_MASK_CD=1 shl SOUND_MIXER_CD;
      SOUND_MASK_IMIX=1 shl SOUND_MIXER_IMIX;
      SOUND_MASK_ALTPCM=1 shl SOUND_MIXER_ALTPCM;
      SOUND_MASK_RECLEV=1 shl SOUND_MIXER_RECLEV;
      SOUND_MASK_IGAIN=1 shl SOUND_MIXER_IGAIN;
      SOUND_MASK_OGAIN=1 shl SOUND_MIXER_OGAIN;
      SOUND_MASK_LINE1=1 shl SOUND_MIXER_LINE1;
      SOUND_MASK_LINE2=1 shl SOUND_MIXER_LINE2;
      SOUND_MASK_LINE3=1 shl SOUND_MIXER_LINE3;
      SOUND_MASK_DIGITAL1=1 shl SOUND_MIXER_DIGITAL1;
      SOUND_MASK_DIGITAL2=1 shl SOUND_MIXER_DIGITAL2;
      SOUND_MASK_DIGITAL3=1 shl SOUND_MIXER_DIGITAL3;
      SOUND_MASK_PHONEIN=1 shl SOUND_MIXER_PHONEIN;
      SOUND_MASK_PHONEOUT=1 shl SOUND_MIXER_PHONEOUT;
      SOUND_MASK_RADIO=1 shl SOUND_MIXER_RADIO;
      SOUND_MASK_VIDEO=1 shl SOUND_MIXER_VIDEO;
      SOUND_MASK_MONITOR=1 shl SOUND_MIXER_MONITOR;
      SOUND_MASK_DEPTH=1 shl SOUND_MIXER_DEPTH;
      SOUND_MASK_CENTER=1 shl SOUND_MIXER_CENTER;
      SOUND_MASK_MIDI=1 shl SOUND_MIXER_MIDI;

      SOUND_MIXER_READ_VOLUME=$80044d00;
      SOUND_MIXER_READ_BASS=$80044d01;
      SOUND_MIXER_READ_TREBLE=$80044d02;
      SOUND_MIXER_READ_SYNTH=$80044d03;
      SOUND_MIXER_READ_PCM=$80044d04;
      SOUND_MIXER_READ_SPEAKER=$80044d05;
      SOUND_MIXER_READ_LINE=$80044d06;
      SOUND_MIXER_READ_MIC=$80044d07;
      SOUND_MIXER_READ_CD=$80044d08;
      SOUND_MIXER_READ_IMIX=$80044d09;
      SOUND_MIXER_READ_ALTPCM=$80044d0a;
      SOUND_MIXER_READ_RECLEV=$80044d0b;
      SOUND_MIXER_READ_IGAIN=$80044d0c;
      SOUND_MIXER_READ_OGAIN=$80044d0d;
      SOUND_MIXER_READ_LINE1=$80044d0e;
      SOUND_MIXER_READ_LINE2=$80044d0f;
      SOUND_MIXER_READ_LINE3=$80044d10;
      SOUND_MIXER_READ_RECSRC=$80044dff;
      SOUND_MIXER_READ_RECMASK=$80044dfd;
      SOUND_MIXER_READ_DEVMASK=$80044dfe;
      SOUND_MIXER_READ_STEREODEVS=$80044dfb;
      SOUND_MIXER_READ_CAPS=$80044dfc;

      SOUND_MIXER_WRITE_VOLUME=$c0044d00;
      SOUND_MIXER_WRITE_BASS=$c0044d01;
      SOUND_MIXER_WRITE_TREBLE=$c0044d02;
      SOUND_MIXER_WRITE_SYNTH=$c0044d03;
      SOUND_MIXER_WRITE_PCM=$c0044d04;
      SOUND_MIXER_WRITE_SPEAKER=$c0044d05;
      SOUND_MIXER_WRITE_LINE=$c0044d06;
      SOUND_MIXER_WRITE_MIC=$c0044d07;
      SOUND_MIXER_WRITE_CD=$c0044d08;
      SOUND_MIXER_WRITE_IMIX=$c0044d09;
      SOUND_MIXER_WRITE_ALTPCM=$c0044d0a;
      SOUND_MIXER_WRITE_RECLEV=$c0044d0b;
      SOUND_MIXER_WRITE_IGAIN=$c0044d0c;
      SOUND_MIXER_WRITE_OGAIN=$c0044d0d;
      SOUND_MIXER_WRITE_LINE1=$c0044d0e;
      SOUND_MIXER_WRITE_LINE2=$c0044d0f;
      SOUND_MIXER_WRITE_LINE3=$c0044d10;
      SOUND_MIXER_WRITE_RECSRC=$c0044dff;
      SOUND_MIXER_INFO=$805c4d65;

      SOUND_MIXER_ACCESS=$c0804d66;
      SOUND_MIXER_GETLEVELS=$c0a44d74;
      SOUND_MIXER_SETLEVELS=$c0a44d75;

      { SOUND_MIXER_GETLEVELS and SOUND_MIXER_SETLEVELS calls can be used
        for querying current mixer settings from the driver and for loading
        default volume settings _prior_ activating the mixer (loading
        doesn't affect current state of the mixer hardware). These calls
        are for internal use only. }

      libcname='libc.so.6';
      O_RDONLY=0;
      O_WRONLY=1;

type audio_buf_info=record
      fragments:integer;  // of available fragments (partially usend ones not counted)
      fragstotal:integer; // Total # of fragments allocated
      fragsize:integer;   // Size of a fragment in bytes
      bytes:integer;      // Available space in bytes (includes partially used fragments)
      // Note! 'bytes' could be more than fragments*fragsize*/
     end;

     count_info=record
      bytes:integer;   // Total # of bytes processed
      blocks:integer; // # of fragment transitions since last time
      ptr:integer;     // Current DMA pointer value }
     end;

     buffmem_desc=record
      buffer:PWord;
      size:integer;
     end;

     audio_errinfo=record
      play_underruns:integer;
      rec_overruns:integer;
      play_ptradjust:Word;
      rec_ptradjust:Word;
      play_errorcount:integer;
      rec_errorcount:integer;
      play_lasterror:integer;
      rec_lasterror:integer;
      play_errorparm:LongInt;
      rec_errorparm:LongInt;
      filler:array[0..15] of integer;
     end;

     oss_digital_control=record
      caps:longword;       // To be defined
      valid :longword;
      cbitin:array[0..23] of Byte;
      ubitin:array[0..23] of Byte;
      cbitout:array[0..23] of Byte;
      ubitout:array[0..23] of Byte;
      outsel:longword;
      in_data:integer;     // Audio/data if autodetectable by the receiver
      in_locked:integer;	  // Receiver locked
      in_quality:integer;  // Input signal quality
      in_vbit,out_vbit:integer;	// V bits
      in_errors:longword;  // Various input errro conditions
     end;

     oss_syncgroup=record
      id:integer;
      mode:integer;
     end;

     mixer_info=record
      id:array[0..15] of char;
      name:array[0..31] of char;
      modify_counter:integer;
      fillers:array[0..9] of integer;
     end;

     mixer_vol_table=record
      num:integer; // Index to volume table
      name:array[0..31] of char;
      levels:array[0..31] of integer;
     end;

function __write(fd:integer;data:pointer;size:integer):integer; cdecl; external libcname;
function __read(Handle:integer;var Buffer;Count:integer):integer; cdecl; external libcname;
function ioctl(fd:integer;command:integer):integer;varargs; cdecl; external libcname;
function open(PathName:pchar;Flags:integer):integer;varargs; cdecl; external libcname;
function __close(Handle:integer):integer; cdecl; external libcname;
function FpSelect(nfds:longint;readfds:pointer;writefds:pointer;exceptfds:pointer;timeout:Ptimeval):longint; cdecl; external libcname name 'select';
{$endif}
{$else}
uses Windows,Messages,MMSystem{$ifdef UseDirectSound},DirectSound9{$endif}{$ifdef sdl},sdl{$endif};
{$endif}
{$endif}

const BeRoXMSampleFixUpLength=4;

      BeRoXMMinPeriod=27;
      BeRoXMMaxPeriod=54784;

      BeRoXMLastSample=255;
      BeRoXMLastInstrument=255;
      BeRoXMLastPattern=255;
      BeRoXMLastPatternOrder=255;
      BeRoXMLastChannel=255;
      BeRoXMLastPatternRow=255;

{$ifdef MixerNoINT64}
      BeRoXMPositionShift=12;
      BeRoXMPositionFactor=1 shl BeRoXMPositionShift;
      BeRoXMPositionMask=BeRoXMPositionFactor-1;
      BeRoXMPositionFactorIncrement=BeRoXMPositionFactor;
      BeRoXMPositionAllRemainBits=14;
      BeRoXMPositionAllRemainFactor=1 shl BeRoXMPositionAllRemainBits;
      BeRoXMPositionRemainBits=(32-1)-BeRoXMPositionShift;
      BeRoXMMaxSampleSize=1 shl BeRoXMPositionRemainBits;
{$else}
{$ifdef MixerIntegerIncrement}
      BeRoXMPositionShift=16;
      BeRoXMPositionFactor=1 shl BeRoXMPositionShift;
      BeRoXMPositionMask=BeRoXMPositionFactor-1;
      BeRoXMPositionFactorIncrement:int64=BeRoXMPositionFactor;
{$else}
      BeRoXMPositionShift=32;
      BeRoXMPositionFactor:int64=$100000000;//(1 shl BeRoXMPositionShift);
      BeRoXMPositionMask=$ffffffff;//INT64(1 shl BeRoXMPositionShift)-1;
      BeRoXMPositionFactorIncrement:int64=$100000000;//(1 shl BeRoXMPositionShift);
{$endif}
      BeRoXMPositionAllRemainBits=14;
      BeRoXMPositionAllRemainFactor=1 shl BeRoXMPositionAllRemainBits;
      BeRoXMMaxSampleSize=$40000000;
{$endif}

      BeRoXMLinearShift=16;
      BeRoXMLinearDiv=1 shl BeRoXMLinearShift;
      BeRoXMLinearPositionShift=BeRoXMPositionShift-BeRoXMLinearShift;
      BeRoXMLinearPositionDiv=1 shl BeRoXMLinearPositionShift;

      BeRoXMCubicSplineValueBits=14;
      BeRoXMCubicSplineValueLength=1 shl BeRoXMCubicSplineValueBits;
      BeRoXMCubicSplineFracBits=10;
      BeRoXMCubicSplineLength=1 shl BeRoXMCubicSplineFracBits;
      BeRoXMCubicSplineFracShift=BeRoXMPositionShift-BeRoXMCubicSplineFracBits;
      BeRoXMCubicSplineFracMask=BeRoXMCubicSplineLength-1;

      BeRoXMWindowedFIRValueBits=15;
      BeRoXMWindowedFIRValueLength=1 shl BeRoXMWindowedFIRValueBits;
      BeRoXMWindowedFIRFracBits=12;
      BeRoXMWindowedFIRLength=1 shl BeRoXMWindowedFIRFracBits;
      BeRoXMWindowedFIRWidthBits=3;
      BeRoXMWindowedFIRWidth=1 shl BeRoXMWindowedFIRWidthBits;
      BeRoXMWindowedFIRCutOff=1; // 0.90;
      BeRoXMWindowedFIRFracShift=BeRoXMPositionShift-BeRoXMWindowedFIRFracBits;
      BeRoXMWindowedFIRFracMask=(1 shl BeRoXMWindowedFIRFracShift)-1;

      BeRoXMMixerBits=16;
      BeRoXMMixerLength=1 shl BeRoXMMixerBits;

      BeRoXMMixerHeadRoomBits=4;

      BeRoXMVolumeBits=BeRoXMMixerBits-BeRoXMMixerHeadRoomBits;
      BeRoXMVolumeLength=1 shl BeRoXMVolumeBits;

      BeRoXMRampBits=12;
      BeRoXMRampLength=1 shl BeRoXMRampBits;

      BeRoXMFilterBits=12;
      BeRoXMFilterLength=1 shl BeRoXMFilterBits;

      BeRoXMClickRemovalFactorBits=8;
      BeRoXMClickRemovalFactorLength=1 shl BeRoXMClickRemovalFactorBits;

      BeRoXMOutHeadRoomDropSaveBits=0;
      BeRoXMOutHeadRoomDropBits=BeRoXMMixerHeadRoomBits-BeRoXMOutHeadRoomDropSaveBits;
      BeRoXMOutBits=BeRoXMMixerBits-BeRoXMOutHeadRoomDropBits;
      BeRoXMOutLength=1 shl BeRoXMOutBits;

      BeRoXMOscillatorLengthBits=8;
      BeRoXMOscillatorLength=1 shl BeRoXMOscillatorLengthBits;
      BeRoXMOscillatorLengthMask=(1 shl BeRoXMOscillatorLengthBits)-1;

      BeRoXMEpsilon:single=1E-12;

      BeRoXMEffectArpeggio=$00;
      BeRoXMEffectPortaUp=$01;
      BeRoXMEffectPortaDown=$02;
      BeRoXMEffectTonePorta=$03;
      BeRoXMEffectVibrato=$04;
      BeRoXMEffectTonePortaVolumeSlide=$05;
      BeRoXMEffectVibratoVolumeSlide=$06;
      BeRoXMEffectTremolo=$07;
      BeRoXMEffectPanning=$08;
      BeRoXMEffectSampleOffset=$09;
      BeRoXMEffectVolumeSlide=$0a;
      BeRoXMEffectPatternJump=$0b;
      BeRoXMEffectVolume=$0c;
      BeRoXMEffectPatternBreak=$0d;
      BeRoXMEffectExtendedEffects=$0e;
      BeRoXMEffectSpeedTempo=$0f;
      BeRoXMEffectGlobalVolume=$10;
      BeRoXMEffectGlobalVolumeSlide=$11;
      BeRoXMEffectKeyOff=$14;
      BeRoXMEffectSetEnvelopePosition=$15;
      BeRoXMEffectPanningSlide=$19;
      BeRoXMEffectResonance=$1a;
      BeRoXMEffectMultiRetrig=$1b;
      BeRoXMEffectTremor=$1d;
      BeRoXMEffectExtraFinePorta=$21;
      BeRoXMEffectMacro=$23;
      BeRoXMEffectSmoothMacro=$25;

      BeRoXMEffectExtendedEffectFinePortaUp=$1;
      BeRoXMEffectExtendedEffectFinePortaDown=$2;
      BeRoXMEffectExtendedEffectSetGlissControl=$3;
      BeRoXMEffectExtendedEffectSetVibratoType=$4;
      BeRoXMEffectExtendedEffectSetFineTune=$5;
      BeRoXMEffectExtendedEffectPatternLoop=$6;
      BeRoXMEffectExtendedEffectSetTremoloType=$7;
      BeRoXMEffectExtendedEffectRetrig=$9;
      BeRoXMEffectExtendedEffectFineVolumeSlideUp=$a;
      BeRoXMEffectExtendedEffectFineVolumeSlideDown=$b;
      BeRoXMEffectExtendedEffectNoteCut=$c;
      BeRoXMEffectExtendedEffectNoteDelay=$d;
      BeRoXMEffectExtendedEffectPatternDelay=$e;
      BeRoXMEffectExtendedEffectSetActiveMacro=$f;

      BeRoXMCutOffLowPass=0;
      BeRoXMCutOffHighPass=1;

      BeROXMRampSamples=128;
      BeROXMFastRampSamples=128;

      BeRoXMStartOffsetFromBeginning=0;
      BeRoXMStartOffsetFromCurrent=1;
      BeRoXMStartOffsetFromEnd=2;

      BeRoXMMixerNearest=0;
      BeRoXMMixerLinear=1;
      BeRoXMMixerCubicSpline=2;
      BeRoXMMixerWindowedFIR=3;

      BeRoXMMixerOutMono=0;
      BeRoXMMixerOutStereo=1;

      BeRoXMMixerInMono=0;
      BeRoXMMixerInStereo=1;

      BeRoXMMixerNoRamping=0;
      BeRoXMMixerRamping=1;

      BeRoXMMixerNoFilter=0;
      BeRoXMMixerLowPass=1;
      BeRoXMMixerHighPass=2;

      BeRoXMMixerMonoPanning=0;
      BeRoXMMixerStereoPanning=1;

      BeRoXMMixerWithoutOscillator=0;
      BeRoXMMixerWithOscillator=1;

      bsoFromBeginning=0;
      bsoFromCurrent=1;
      bsoFromEnd=2;

type plongint=^longint;
     pinteger=^integer;
     pshortint=^shortint;
     psingle=^single;

{$ifdef windows}
     TWAVEHDR=WAVEHDR;
{$endif}

{$ifndef fpc}
     ptruint=longword;
     ptrint=longint;
     TThreadID=longword;
{$endif}

     PBeRoXMByteArray=^TBeRoXMByteArray;
     TBeRoXMByteArray=array[0..(2147483647 div sizeof(byte))-1] of byte;

     PBeRoXMChunkSignature=^TBeRoXMChunkSignature;
     TBeRoXMChunkSignature=array[0..3] of char;

     PBeRoXMRingBuffer=^TBeRoXMRingBuffer;
     TBeRoXMRingBuffer=record
      Data:PBeRoXMByteArray;
      ReadPos:longword;
      WritePos:longword;
      Size:longword;
     end;
    
{$ifdef MixerNoINT64}
     TBeRoXMMixerVariable=integer;
{$else}
     TBeRoXMMixerVariable=int64;
{$endif}

     TBeRoXMSoundOutputMode=(somNone{$ifdef windows},somWaveOut{$ifdef UseDirectSound},somDirectSound{$endif}{$endif}{$ifdef unix}{$ifndef android},somDevDSP{$endif}{$endif}{$ifdef sdl},somSDL{$endif}{$ifdef BeRoXMRingBuffer},somRingBuffer{$endif});

     PBeRoXMStereoSample=^TBeRoXMStereoSample;
     TBeRoXMStereoSample=packed record
      Left,Right:integer;
     end;

     TBeRoXMResamplingMethod=BeRoXMMixerNearest..BeRoXMMixerWindowedFIR;

     PBeRoXMStreamData=^TBeRoXMStreamData;
     TBeRoXMStreamData=packed array[0..$7ffffffe] of byte;

     PBeRoStreamBuffer=^TBeRoXMStreamBuffer;
     TBeRoXMStreamBuffer=packed array[1..4096] of byte;

     TBeRoXMStream=class
      private
       fPosition,fSize,fInMemorySize:integer;
       fData:PBeRoXMStreamData;
       fBitBuffer:longword;
       fBitBufferSize:byte;
       procedure Realloc(NewInMemorySize:integer);
       procedure Resize(NewSize:integer);
       function GetString:string;
       procedure setstring(Value:string);
       function GetByte(BytePosition:integer):byte;
       procedure SetByte(BytePosition:integer;Value:byte);
      public
       constructor Create;
       destructor Destroy; override;
       function Assign(Src:TBeRoXMStream):integer;
       function Append(Src:TBeRoXMStream):integer;
       function AppendFrom(Src:TBeRoXMStream;Counter:integer):integer;
       procedure Clear; virtual;
       function Read(var Buf;Count:integer):integer; virtual;
       function ReadAt(Position:integer;var Buf;Count:integer):integer; virtual;
       function Write(const Buf;Count:integer):integer; virtual;
       function SeekEx(APosition:integer):integer; virtual;
       function Seek(APosition:integer):integer; overload;
       function Seek(APosition,Origin:integer):integer; overload;
       function Position:integer; virtual;
       function Size:integer; virtual;
       procedure SetSize(NewSize:integer);
       function ReadByte:byte;
       function ReadWord:word;
       function ReadDWord:longword;
       function ReadLine:string;
       function ReadString:string;
       function ReadWideString:widestring;
       procedure WriteByte(Value:byte);
       function WriteByteCount(Value:byte;Count:integer):integer;
       procedure WriteWord(Value:word);
       procedure WriteDWord(Value:longword);
       procedure WriteShortInt(Value:shortint);
       procedure WriteSmallInt(Value:smallint);
       procedure WriteLongInt(Value:longint);
       procedure WriteInt64(Value:int64);
       procedure WriteBoolean(Value:boolean);
       procedure WriteLine(Line:string);
       procedure WriteString(S:string);
       procedure WriteDataString(S:string);
       procedure WriteDataWideString(S:widestring);
       procedure ResetBits;
       function ReadBit:boolean;
       function ReadBits(BitsCount:byte):longword;
       function ReadBitsSigned(BitsCount:byte):longint;
       procedure WriteBit(Value:boolean);
       procedure WriteBits(Value:longword;BitsCount:byte);
       procedure WriteBitsSigned(Value:longint;BitsCount:byte);
       procedure FlushBits;
       property Text:string Read GetString Write setstring;
       property Bytes[BytePosition:integer]:byte Read GetByte Write SetByte; default;
       property BitsInBuffer:byte Read fBitBufferSize;
     end;

     TBeRoXMMemoryStream=TBeRoXMStream;

     TBeRoXMFileStream=class(TBeRoXMStream)
      private
       fFile:file;
      public
       constructor Create(FileName:string);
       constructor CreateNew(FileName:string);
       destructor Destroy; override;
       function Read(var Buf;Count:integer):integer; override;
       function Write(const Buf;Count:integer):integer; override;
       function SeekEx(APosition:integer):integer; override;
       function Position:integer; override;
       function Size:integer; override;
     end;

     PBeRoXMMIDIString=^TBeRoXMMIDIString;
     TBeRoXMMIDIString=array[0..32-1] of char;

     PBeRoXMMIDIConfig=^TBeRoXMMIDIConfig;
     TBeRoXMMIDIConfig=packed record
      Global:array[0..9-1] of TBeRoXMMIDIString;
      SFXExt:array[0..16-1] of TBeRoXMMIDIString;
      ZXXExt:array[0..128-1] of TBeRoXMMIDIString;
     end;

     PBeRoXMBuffer=^TBeRoXMBuffer;
     TBeRoXMBuffer=array[0..($7fffffff div sizeof(longint))-1] of longint;

     PBeRoXMSampleValue=^TBeRoXMSampleValue;
     TBeRoXMSampleValue=integer;

     PBeRoXMSampleData=^TBeRoXMSampleData;
     TBeRoXMSampleData=array[0..($7fffffff div sizeof(TBeRoXMSampleValue))-1] of TBeRoXMSampleValue;

     TBeRoXMDPCM4Table=array[0..15] of shortint;

     TBeRoXM=class;
     TBeRoXMModule=class;

     TBeRoXMPatternNote=record
      Note,Instrument,Volume,Effect,EffectParameter:integer;
     end;

     TBeRoXMAudioProcessChain=class;

     TBeRoXMAudioProcessChainItem=class
      public
       Chain:TBeRoXMAudioProcessChain;
       Previous,Next:TBeRoXMAudioProcessChainItem;
       constructor Create(TheChain:TBeRoXMAudioProcessChain); virtual;
       destructor Destroy; override;
       procedure Reset; virtual;
       procedure Process; virtual;
       function LoadFromStream(Stream:TBeRoXMStream):boolean; virtual;
       function SaveToStream(Stream:TBeRoXMStream):boolean; virtual;
     end;

     TBeRoXMAudioProcessChainItemUnknown=class(TBeRoXMAudioProcessChainItem)
      public
       ChunkSignature:TBeRoXMChunkSignature;
       Data:TBeRoXMStream;
       constructor Create(TheChain:TBeRoXMAudioProcessChain); override;
       destructor Destroy; override;
       function LoadFromStream(Stream:TBeRoXMStream):boolean; override;
       function SaveToStream(Stream:TBeRoXMStream):boolean; override;
     end;

     TBeRoXMAudioProcessChain=class
      private
       procedure ProcessClickRemoval(LengthCounter:integer);
      public
       Module:TBeRoXMModule;
       Buffer:PBeRoXMBuffer;
       BufferSamples:integer;
       BufferChannels:integer;
       BufferSize:integer;
       Active:bytebool;
       Mix:bytebool;
       Name:string;
       LastLeft,LastRight:integer;
       First,Last:TBeRoXMAudioProcessChainItem;
       constructor Create(TheModule:TBeRoXMModule;TheBufferSamples:integer;TheBufferChannels:integer);
       destructor Destroy; override;
       procedure Clear;
       procedure Reset;
       procedure ResetBuffer(Samples:integer);
       procedure ProcessBuffer(StartPosition,Samples:integer);
       procedure Append(Item:TBeRoXMAudioProcessChainItem);
       procedure Prepend(Item:TBeRoXMAudioProcessChainItem);
       procedure InsertAfter(Item,OtherItem:TBeRoXMAudioProcessChainItem);
       procedure InsertBefore(Item,OtherItem:TBeRoXMAudioProcessChainItem);
       procedure Remove(Item:TBeRoXMAudioProcessChainItem);
       function LoadFromStream(Stream:TBeRoXMStream):boolean; virtual;
       function SaveToStream(Stream:TBeRoXMStream):boolean; virtual;
     end;

     TBeRoXMAudioProcessChains=array[byte] of TBeRoXMAudioProcessChain;

     TBeRoXMSample=class
      public
       Data:PBeRoXMSampleData;
       SampleLength:integer;
       Bits:integer;
       Channels:integer;
{$ifdef UseDPCM4}
       DPCM4:boolean;
{$endif}
       Name:string;
       Loop:boolean;
       PingPongLoop:boolean;
       LoopStart:integer;
       LoopLength:integer;
       Volume:integer;
       DoPanning:boolean;
       Panning:integer;
       RelativeNote:integer;
       FineTune:integer;
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       procedure Resize(Samples:integer);
       procedure FixUp;
     end;

     TBeRoXMEnvelopePoint=record
      Tick:integer;
      Value:integer;
     end;

     TBeRoXMEnvelope=class
      public
       Active:boolean;
       Loop:boolean;
       Sustain:boolean;
       Points:array[0..12] of TBeRoXMEnvelopePoint;
       NumberOfPoints:integer;
       LoopStartPoint:integer;
       LoopEndPoint:integer;
       SustainPoint:integer;
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
     end;
     TBeRoXMSamples=array[0..BeRoXMLastSample] of TBeRoXMSample;

     TBeRoXMInstrumentSampleMap=array[1..96] of byte;

     TBeRoXMInstrument=class
      public
       Name:string;
       InstrumentType:integer;
       Sample:TBeRoXMSamples;
       SampleMap:TBeRoXMInstrumentSampleMap;
       VolumeEnvelope:TBeRoXMEnvelope;
       PanningEnvelope:TBeRoXMEnvelope;
       FadeOut:integer;
       VibratoType:integer;
       VibratoSweep:integer;
       VibratoDepth:integer;
       VibratoRate:integer;
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
     end;
     TBeRoXMInstruments=array[1..BeRoXMLastInstrument] of TBeRoXMInstrument;

     TBeRoXMEnvelopeHandler=class
      public
       Envelope:TBeRoXMEnvelope;
       KeyOn:boolean;
       CurrentTick:integer;
       CurrentPoint:integer;
       NextPoint:integer;
       CurrentValue:integer;
       Delta:integer;
       constructor Create;
       destructor Destroy; override;
       procedure Assign(From:TBeRoXMEnvelopeHandler);
       procedure Reset(TheTick:integer);
       function ProcessTick:integer;
     end;

     TBeRoXMChannelArpeggioPeriodDelta=array[0..2] of integer;

{$ifdef ChannelOscillators}
     TBeRoXMOscillatorBuffer=array[0..BeRoXMOscillatorLength-1] of TBeRoXMStereoSample;
{$endif}

     TBeRoXMChannel=class
      private
       IsVirtualChannel,IsClickRemovalFadeOutChannel:boolean;
      public
       Name:string;
       Active:boolean;
       Module:TBeRoXMModule;
       Number:integer;
       Plugin:integer;
       AudioProcessChain:byte;
       Sample:TBeRoXMSample;
       Instrument:TBeRoXMInstrument;
       Increment:TBeRoXMMixerVariable;
       SampleIncrement:TBeRoXMMixerVariable;
       SamplePosition:TBeRoXMMixerVariable;
{$ifdef MixerIntegerIncrement}
       SampleIntegerIncrement:integer;
       SampleIntegerPosition:integer;
       SampleIntegerSubPosition:integer;
{$endif}
       SampleBackwards:boolean;
       MixerVolume:integer;
       MixerPanning:integer;
       MixerFrequency:integer;
       EnvelopeVolume:integer;
       EnvelopePanning:integer;
       VolumeEnvelope:TBeRoXMEnvelopeHandler;
       PanningEnvelope:TBeRoXMEnvelopeHandler;
       CurrentNote:TBeRoXMPatternNote;
       Tick:integer;
       VibratoType,VibratoSpeed,VibratoDepth,VibratoPosition:integer;
       TremoloSpeed,TremoloDepth,TremoloPosition:integer;
       TremorPosition,TremorOn,TremorOff:integer;
       InstrumentVibratoSweepPosition,InstrumentVibratoPosition:integer;
       MultiRetrigAction,MultiRetrigOnTick:integer;
       Period:integer;
       ArpeggioPeriodDelta:TBeRoXMChannelArpeggioPeriodDelta;
       PeriodDelta:integer;
       DestPeriod:integer;
       Volume:integer;
       StandardPanning:integer;
       Panning:integer;
       FadeOut:integer;
       VolumeSlideParameter:integer;
       GlobalVolumeSlideParameter:integer;
       TonePortaParameter:integer;
       PortaUpParameter:integer;
       PortaDownParameter:integer;
       FinePortaUpParameter:integer;
       FinePortaDownParameter:integer;
       ExtraFinePortaUpParameter:integer;
       ExtraFinePortaDownParameter:integer;
       FineVolumeUpParameter:integer;
       FineVolumeDownParameter:integer;
       PanningSlideParameter:integer;
       VolumeDelta:integer;
       Glissando:boolean;
       TremoloType:integer;
       FineTune:integer;
       KeyOff:boolean;
       PatternLoopStart:integer;
       PatternLoopCounter:integer;
       PatternLoop:boolean;
       LastLeft,LastRight:integer;
       VolumeLeft,VolumeRight:integer;
       VolumeRampingLeft,VolumeRampingRight:integer;
       DestVolumeRampingLeft,DestVolumeRampingRight:integer;
       StepVolumeRampingLeft,StepVolumeRampingRight:integer;
       VolumeRampingCounter:integer;
       RealNote:integer;
       CutOff:integer;
       Resonance:integer;
       ActiveMacro:integer;
       FilterActive:boolean;
       FilterMode:integer;
       FilterA0,FilterB0,FilterB1:integer;
       FilterY1,FilterY2,FilterY3,FilterY4:integer;
       SmoothInitialValue,SmoothValueStep:single;
       Surround:boolean;
       LocalFilterMode:boolean; 
{$ifdef ChannelOscillators}
       OscillatorBuffer:TBeRoXMOscillatorBuffer;
       OscillatorPosition:integer;
{$endif}
       constructor Create(TheModule:TBeRoXMModule;TheNumber:integer;VirtualChannel,ClickRemovalFadeOutChannel:boolean);
       destructor Destroy; override;
       procedure Assign(From:TBeRoXMChannel);
       procedure Clear;
       procedure Reset;
       function StartClickRemovalFadeOut:boolean;
       procedure UpdateArpeggio;
       procedure UpdateAutoVibrato;
       procedure UpdateVibrato;
       procedure UpdateTremolo;
       procedure UpdateTremor;
       procedure UpdateTonePorta;
       procedure UpdateVolumeSlide;
{$ifdef UseMIDIMacros}
       procedure UpdateMacro(Effect,EffectParameter:integer);
{$endif}
       procedure UpdateEnvelopes;
       procedure UpdateTick;
       procedure ProcessRow(const Note:TBeRoXMPatternNote);
       procedure ProcessTick(const Note:TBeRoXMPatternNote);
       procedure ProcessTrigger(const Note:TBeRoXMPatternNote);
{$ifdef UseFilters}
{$ifndef UseFilterCoefTables}
       function CutOffToFrequency(CutOffValue,FilterModifierValue:integer):integer;
{$endif}
       procedure SetCutOffFilter(FilterReset:boolean);
{$endif}
{$ifdef UseMIDIMacros}
       procedure ProcessMacro(var MIDIString:TBeRoXMMIDIString;Parameter:integer;Smooth:boolean);
{$endif}
     end;
     TBeRoXMChannels=array[0..BeRoXMLastChannel] of TBeRoXMChannel;

     TBeRoXMPatternNotes=array of TBeRoXMPatternNote;

     TBeRoXMPattern=class
      public
       Name:string;
       Pattern:TBeRoXMPatternNotes;
       Rows:integer;
       Channels:byte;
       constructor Create;
       destructor Destroy; override;
       procedure Assign(Source:TBeRoXMPattern);
       procedure Clear;
       procedure ClearPattern;
       function GetNote(Row,Channel:integer):TBeRoXMPatternNote;
       procedure SetNote(Row,Channel:integer;Note:TBeRoXMPatternNote);
       procedure Resize(NewRows:integer;NewChannels:byte);
       function IsEmpty:boolean;
     end;
     TBeRoXMPatterns=array[0..BeRoXMLastPattern] of TBeRoXMPattern;

     TBeRoXMPatternOrder=array[0..BeRoXMLastPatternOrder] of byte;

     TBeRoXMModule=class
      public
       Owner:TBeRoXM;
       Name:string;
       Comment:string;
       Instrument:TBeRoXMInstruments;
       Channel:TBeRoXMChannels;
       NewNoteActionChannel:TBeRoXMChannels;
       ClickRemovalFadeOutChannel:TBeRoXMChannels;
       Pattern:TBeRoXMPatterns;
       PatternOrder:TBeRoXMPatternOrder;
       GlobalVolume:integer;
       MasterVolume:integer;
       Tick:integer;
       PatternDelay:integer;
       Speed:byte;
       Tempo:byte;
       StartSpeed:byte;
       StartTempo:byte;
       NextPatternOrder:integer;
       NextPatternRow:integer;
       TrackEnd:boolean;
       Jump:boolean;
       SampleRate:integer;
       CurrentPatternOrder:integer;
       CurrentPatternRow:integer;
       CurrentPattern:byte;
       TrackLength:integer;
       RestartPosition:integer;
       LinearSlides:boolean;
       ExtendedFilterRange:boolean;
       NumberOfChannels:integer;
       IsTrackActive:boolean;
       LastLeft,LastRight:integer;
       TickSamples:integer;
{$ifdef UseMIDIMacros}
       MIDIConfig:TBeRoXMMIDIConfig;
       EmbeddedMIDIConfig:boolean;
{$endif}
       RampSamples:integer;
       FastRampSamples:integer;
       BufferSize:integer;
       EQFXData,ModularData:TBeRoXMMemoryStream;
       FXData:array[0..99] of TBeRoXMMemoryStream;
       AudioProcessChains:TBeRoXMAudioProcessChains;
       constructor Create(AOwner:TBeRoXM);
       destructor Destroy; override;
       procedure Clear;
       procedure Reset;
       procedure ProcessTick;
       function GetPeriod(Note,FineTune:integer):integer;
       function GetNote(Period,FineTune:integer):integer;
       function GetFrequency(Period:integer):integer;
       procedure SetMIDIString(var MIDIString:TBeRoXMMIDIString;S:string);
{$ifdef UseMIDIMacros}
       procedure ResetMIDIConfig;
{$endif}
       function Load(DataPointer:pointer;DataSize:longword):boolean; overload;
       function Load(Data:TBeRoXMStream):boolean; overload;
       function LoadFile(FileName:string):boolean;
       function LoadFromResource(Instance:THandle;const ResName:string;ResType:pchar):boolean;
{$ifdef UseSaver}
       function Save(Data:TBeRoXMStream;SampleCompressionThreshold:integer=100):boolean;
       function SaveFile(FileName:string;SampleCompressionThreshold:integer=100):boolean;
{$endif}
     end;

{$ifdef MixerCubicSpline}
     PBeRoXMCubicSplineSubArray=^TBeRoXMCubicSplineSubArray;
     TBeRoXMCubicSplineSubArray=packed array[0..3] of integer;

     PBeRoXMCubicSplineArray=^TBeRoXMCubicSplineArray;
     TBeRoXMCubicSplineArray=packed array[0..BeRoXMCubicSplineLength-1] of TBeRoXMCubicSplineSubArray;

     TBeRoXMCubicSpline=class
      public
       Table:TBeRoXMCubicSplineArray;
       constructor Create;
       destructor Destroy; override;
     end;
{$endif}

{$ifdef MixerWindowedFIR}
     PBeRoXMWindowedFIRSubArray=^TBeRoXMWindowedFIRSubArray;
     TBeRoXMWindowedFIRSubArray=packed array[0..BeRoXMWindowedFIRWidth-1] of integer;

     PBeRoXMWindowedFIRArray=^TBeRoXMWindowedFIRArray;
     TBeRoXMWindowedFIRArray=packed array[0..BeRoXMWindowedFIRLength-1] of TBeRoXMWindowedFIRSubArray;

     TBeRoXMWindowedFIR=class
      public
       Table:TBeRoXMWindowedFIRArray;
       constructor Create;
       destructor Destroy; override;
     end;
{$endif}

{$ifdef UseFilters}
{$ifdef UseFilterCoefTables}
     TBeRoXMFilterCoef=packed record
      A0,B0,B1:integer;
     end;

     TBeRoXMFilterCoefs=array[0..127,0..127] of TBeRoXMFilterCoef;
{$endif}
{$endif}

     TBeRoXM=class
      private
       OutputSampleRate:longword;
       OutputChannels:integer;
       OutputBits:integer;
       BufferMul:integer;
       BufferSamples:integer;
       WorkBufferSize:integer;
       OutputBufferSize:integer;
       TotalBufferSize:integer;
       BufferCount:integer;
       PrebufferCount:integer;
{$ifdef windows}
       WaveFormat:TWaveFormatEx;
       WaveOutHandle:longword;
       WaveHandler:array of PWAVEHDR;
       WaveOutIsSampleAccurate:boolean;
       WaveOutLastGetPositionSample:int64;
       WaveOutAddGetPositionSample:int64;
       WaveOutFirstBufferTime:int64;
{$ifdef UseDirectSound}
       DirectSound:IDirectSound;
       DirectSoundBuffer:IDirectSoundBuffer;
       DirectSoundBufferPrimary:IDirectSoundBuffer;
       DSBufferDesc:TDSBufferDesc;
       DirectSoundBasePosition:int64;
{$endif}
{$endif}
{$ifdef unix}
{$ifndef android}
       DevDSPFileHandle:integer;
       DevDSPBuffer:pointer;
{$endif}
{$endif}
{$ifdef sdl}
       SDLWaveFormat:TSDL_AudioSpec;
{$endif}
{$ifdef BeRoXMRingBuffer}
{$endif}
       DSPRingBuffer:pointer;
       DSPRingBufferPosition:longword;
       DSPRingBufferMaxPosition:longword;
       BuffersPrebuffered:integer;
       SleepTime:integer;
       BufferCounter:integer;
       ThreadCriticalSection:TRTLCriticalSection;
{$ifndef android}
       Threaded:boolean;
       ThreadHandle:THandle;
       ThreadID:TThreadID;
       ThreadTerminated:boolean;
       ThreadSuspended:boolean;
       ThreadIsSuspended:boolean;
       ThreadResumeSemaphore:pointer;
       ThreadSuspendSemaphore:pointer;
       ThreadActiveSemaphore:pointer;
{$endif}
       Start:boolean;
       TickSamples:longword;
       TickSamplesCounter:longword;
       MixBufferData:pointer;
       Buffer:PBeRoXMBuffer;
{$ifdef MixerCubicSpline}
       CubicSpline:TBeRoXMCubicSpline;
{$endif}
{$ifdef MixerWindowedFIR}
       WindowedFIR:TBeRoXMWindowedFIR;
{$endif}
{$ifdef UseFilters}
{$ifdef UseFilterCoefTables}
       FilterCoefs:TBeRoXMFilterCoefs;
{$endif}
{$endif}
       ClickRemovalFactor:integer;
       SoundOutputMode:TBeRoXMSoundOutputMode;
{$ifdef windows}
{$ifdef UseDirectSound}
       OwnerWindowHandle:THANDLE;
       function InitDirectSound:boolean;
{$endif}
       function InitWaveOut:boolean;
{$endif}
{$ifdef unix}
{$ifndef android}
       function InitDevDSP:boolean;
{$endif}
{$endif}
{$ifdef sdl}
       function InitSDL:boolean;
{$endif}
{$ifdef BeRoXMRingBuffer}
       function InitRingBuffer:boolean;
{$endif}
       procedure ClearData;
{$ifdef UseFilters}
{$ifdef UseFilterCoefTables}
       procedure FilterCoefCalculateTable(SampleRate:integer);
{$endif}
{$endif}
       procedure SetTickVariables;
       procedure ProcessTick;
       procedure ResetSound;
       function GetSampleLength(Channel:TBeRoXMChannel;CountSamplesValue:TBeRoXMMixerVariable):TBeRoXMMixerVariable;
       procedure MixChannel(Channel:TBeRoXMChannel;StartPosition,LengthCounter:integer);
       procedure ProcessClickRemoval(StartPosition,LengthCounter:integer);
       function DoMix(StartPosition,LengthCounter:integer;var DoContinue:boolean;NewTick:boolean):longword;
       procedure IncOutputSamples;
      public
       BufferSize:integer;
       Module:TBeRoXMModule;
       ResamplingMethod:TBeRoXMResamplingMethod;
       Clipping:boolean;
       Oscillators:boolean;
       Looping:boolean;
       RingBuffer:TBeRoXMRingBuffer;
       OutputSamples:int64;
       Playing:boolean;
       constructor Create(TheOutputSampleRate:longword;TheBufferSize,TheBufferCount:integer;Stereo:boolean;TheSleepTime:integer{$ifdef UseDirectSound};WindowHandle:THANDLE{$endif};TheBits:integer=16;ThePrebufferCount:integer=0);
       destructor Destroy; override;
       procedure Enter;
       procedure Leave;
       procedure Clear;
       function Play:boolean;
       procedure Stop;
       procedure Poll;
       procedure GetBuffer(DestBuffer:pointer;Bytes:longword);
       procedure GetBufferSamples(DestBuffer:pointer;Samples:longword);
       function GetTimePosition:int64;
       function GetTimeDuration(Factor:int64=1;Position:int64=-1):int64;
       function SeekToTimePosition(Factor:int64=1;Position:int64=-1):int64;
       procedure FillBuffer;
       procedure MixTo(DestBuffer:pointer;Volume:longint);
       procedure DownMix(DestBuffer:pointer);
       procedure MixBuffer(DestBuffer:pointer);
     end;

{$ifdef BeRoXMRingBuffer}
procedure CreateRingBuffer(var RingBuffer:TBeRoXMRingBuffer;Size:longword);
procedure DestroyRingBuffer(var RingBuffer:TBeRoXMRingBuffer);
function RingBufferUsed(var RingBuffer:TBeRoXMRingBuffer):longword;
function RingBufferSpace(var RingBuffer:TBeRoXMRingBuffer):longword;
function ReadFromRingBuffer(var RingBuffer:TBeRoXMRingBuffer;Buffer:pointer;Bytes:longword):longword;
function WriteToRingBuffer(var RingBuffer:TBeRoXMRingBuffer;Buffer:pointer;Bytes:longword):longword;
procedure ResetRingBuffer(var RingBuffer:TBeRoXMRingBuffer);
{$endif}

implementation

const MIDIOUT_START=0;
      MIDIOUT_STOP=1;
      MIDIOUT_TICK=2;
      MIDIOUT_NOTEON=3;
      MIDIOUT_NOTEOFF=4;
      MIDIOUT_VOLUME=5;
      MIDIOUT_PAN=6;
      MIDIOUT_BANKSEL=7;
      MIDIOUT_PROGRAM=8;

type TXMHeader=packed record
      Signature:array[0..16] of char;
      Name:array[0..19] of char;
      End1AValue:byte;
      Tracker:array[0..19] of char;
      Version:word;
      Size:longword;
      TrackLength:word;
      RestartPosition:word;
      NumberOfChannels:word;
      NumberOfPatterns:word;
      NumberOfInstruments:word;
      Flags:word;
      Speed:word;
      Tempo:word;
      PatternOrder:TBeRoXMPatternOrder;
     end;

     TXMInstrumentHeader=packed record
      Size:longword;
      Name:packed array[0..22-1] of char;
      InstrumentType:byte;
      CountOfSamples:word;
     end;

     TXMPatternHeader=packed record
      Size:longword;
      PackingType:byte;
      Rows:word;
      PackedSize:word;
     end;

     TXMOldPatternHeader=packed record
      Size:longword;
      PackingType:byte;
      Rows:byte;
      PackedSize:word;
     end;

     TXMInstrumentExtraHeader=packed record
      Size:longword;
      SampleMap:packed array[1..96] of byte;
      VolumeEnvelope:packed array[0..24-1] of word;
      PanningEnvelope:packed array[0..24-1] of word;
      VolumeEnvelopeNumPoints:byte;
      PanningEnvelopeNumPoints:byte;
      VolumeEnvelopeSustain:byte;
      VolumeEnvelopeLoopStart:byte;
      VolumeEnvelopeLoopEnd:byte;
      PanningEnvelopeSustain:byte;
      PanningEnvelopeLoopStart:byte;
      PanningEnvelopeLoopEnd:byte;
      VolFlags:byte;
      PanFlags:byte;
      VibratoType:byte;
      VibratoSweep:byte;
      VibratoDepth:byte;
      VibratoRate:byte;
      FadeOut:word;
      Reserved:word;
     end;

     TXMSampleHeader=packed record
      SampleLength:longword;
      LoopStart:longword;
      LoopLength:longword;
      Volume:byte;
      FineTune:shortint;
      Flags:byte;
      Panning:byte;
      RelativeNote:shortint;
      Reserved:byte;
      Name:packed array[0..22-1] of char;
     end;

     TMODSample=packed record
      Name:array[0..21] of char;
      LengthCounter:word;
      FineTune:byte;
      Volume:byte;
      LoopStart:word;
      LoopLength:word;
     end;

     TMODHeader15=packed record
      Title:array[0..19] of char;
      Samples:array[1..15] of TMODSample;
      TrackLength:byte;
      RestartPosition:byte;
      PatternOrder:array[0..127] of byte;
     end;

     TMODHeader31=packed record
      Title:array[0..19] of char;
      Samples:array[1..31] of TMODSample;
      TrackLength:byte;
      RestartPosition:byte;
      PatternOrder:array[0..127] of byte;
      Tag:array[0..3] of char;
     end;

{$ifdef UseTables}
const ModulationTable:array[0..4-1,0..64-1] of shortint=
       ((0,12,25,37,49,60,71,81,90,98,106,112,117,122,125,126,                                  // Sinus
         127,126,125,122,117,112,106,98,90,81,71,60,49,37,25,12,
         0,-12,-25,-37,-49,-60,-71,-81,-90,-98,-106,-112,-117,-122,-125,-126,
	       -127,-126,-125,-122,-117,-112,-106,-98,-90,-81,-71,-60,-49,-37,-25,-12),
        (0,-4,-8,-12,-16,-20,-24,-28,-32,-36,-40,-44,-48,-52,-56,-60,                           // Ramp down
         -64,-68,-72,-76,-80,-84,-88,-92,-96,-100,-104,-108,-112,-116,-120,-124,
         127,123,119,115,111,107,103,99,95,91,87,83,79,75,71,67,
         63,59,55,51,47,43,39,35,31,27,23,19,15,11,7,3),
        (127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,                       // Square
         127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,127,
         -127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,
         -127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127,-127),
        (88,-127,-43,88,102,41,-65,-94,125,20,-71,-86,-70,-32,-16,-96,                          // Random
         17,72,107,-5,116,-69,-62,-40,10,-61,65,109,-18,-38,-13,-76,
         -23,88,21,-94,8,106,21,-112,6,109,20,-88,-30,9,-127,118,
         42,-34,89,-4,-51,-72,21,-29,112,123,84,-101,-92,98,-54,-95));

      AutoVibratoSinusTable:array[0..256-1] of shortint=
       (0,-2,-3,-5,-6,-8,-9,-11,-12,-14,-16,-17,-19,-20,-22,-23,
        -24,-26,-27,-29,-30,-32,-33,-34,-36,-37,-38,-39,-41,-42,
        -43,-44,-45,-46,-47,-48,-49,-50,-51,-52,-53,-54,-55,-56,
        -56,-57,-58,-59,-59,-60,-60,-61,-61,-62,-62,-62,-63,-63,
        -63,-64,-64,-64,-64,-64,-64,-64,-64,-64,-64,-64,-63,-63,
        -63,-62,-62,-62,-61,-61,-60,-60,-59,-59,-58,-57,-56,-56,
        -55,-54,-53,-52,-51,-50,-49,-48,-47,-46,-45,-44,-43,-42,
        -41,-39,-38,-37,-36,-34,-33,-32,-30,-29,-27,-26,-24,-23,
        -22,-20,-19,-17,-16,-14,-12,-11,-9,-8,-6,-5,-3,-2,0,
        2,3,5,6,8,9,11,12,14,16,17,19,20,22,23,24,26,27,29,30,
        32,33,34,36,37,38,39,41,42,43,44,45,46,47,48,49,50,51,
        52,53,54,55,56,56,57,58,59,59,60,60,61,61,62,62,62,63,
        63,63,64,64,64,64,64,64,64,64,64,64,64,63,63,63,62,62,
        62,61,61,60,60,59,59,58,57,56,56,55,54,53,52,51,50,49,
        48,47,46,45,44,43,42,41,39,38,37,36,34,33,32,30,29,27,
        26,24,23,22,20,19,17,16,14,12,11,9,8,6,5,3,2);

      XMPeriodTable:array[0..104-1] of word=
       (907,900,894,887,881,875,868,862,856,850,844,838,832,826,820,814,
        808,802,796,791,785,779,774,768,762,757,752,746,741,736,730,725,
        720,715,709,704,699,694,689,684,678,675,670,665,660,655,651,646,
        640,636,632,628,623,619,614,610,604,601,597,592,588,584,580,575,
        570,567,563,559,555,551,547,543,538,535,532,528,524,520,516,513,
        508,505,502,498,494,491,487,484,480,477,474,470,467,463,460,457,
        453,450,447,443,440,437,434,431);

      XMLinearTable:array[0..768-1] of longword=
       (535232,534749,534266,533784,533303,532822,532341,531861,
        531381,530902,530423,529944,529466,528988,528511,528034,
        527558,527082,526607,526131,525657,525183,524709,524236,
        523763,523290,522818,522346,521875,521404,520934,520464,
        519994,519525,519057,518588,518121,517653,517186,516720,
        516253,515788,515322,514858,514393,513929,513465,513002,
        512539,512077,511615,511154,510692,510232,509771,509312,
        508852,508393,507934,507476,507018,506561,506104,505647,
        505191,504735,504280,503825,503371,502917,502463,502010,
        501557,501104,500652,500201,499749,499298,498848,498398,
        497948,497499,497050,496602,496154,495706,495259,494812,
        494366,493920,493474,493029,492585,492140,491696,491253,
        490809,490367,489924,489482,489041,488600,488159,487718,
        487278,486839,486400,485961,485522,485084,484647,484210,
        483773,483336,482900,482465,482029,481595,481160,480726,
        480292,479859,479426,478994,478562,478130,477699,477268,
        476837,476407,475977,475548,475119,474690,474262,473834,
        473407,472979,472553,472126,471701,471275,470850,470425,
        470001,469577,469153,468730,468307,467884,467462,467041,
        466619,466198,465778,465358,464938,464518,464099,463681,
        463262,462844,462427,462010,461593,461177,460760,460345,
        459930,459515,459100,458686,458272,457859,457446,457033,
        456621,456209,455797,455386,454975,454565,454155,453745,
        453336,452927,452518,452110,451702,451294,450887,450481,
        450074,449668,449262,448857,448452,448048,447644,447240,
        446836,446433,446030,445628,445226,444824,444423,444022,
        443622,443221,442821,442422,442023,441624,441226,440828,
        440430,440033,439636,439239,438843,438447,438051,437656,
        437261,436867,436473,436079,435686,435293,434900,434508,
        434116,433724,433333,432942,432551,432161,431771,431382,
        430992,430604,430215,429827,429439,429052,428665,428278,
        427892,427506,427120,426735,426350,425965,425581,425197,
        424813,424430,424047,423665,423283,422901,422519,422138,
        421757,421377,420997,420617,420237,419858,419479,419101,
        418723,418345,417968,417591,417214,416838,416462,416086,
        415711,415336,414961,414586,414212,413839,413465,413092,
        412720,412347,411975,411604,411232,410862,410491,410121,
        409751,409381,409012,408643,408274,407906,407538,407170,
        406803,406436,406069,405703,405337,404971,404606,404241,
        403876,403512,403148,402784,402421,402058,401695,401333,
        400970,400609,400247,399886,399525,399165,398805,398445,
        398086,397727,397368,397009,396651,396293,395936,395579,
        395222,394865,394509,394153,393798,393442,393087,392733,
        392378,392024,391671,391317,390964,390612,390259,389907,
        389556,389204,388853,388502,388152,387802,387452,387102,
        386753,386404,386056,385707,385359,385012,384664,384317,
        383971,383624,383278,382932,382587,382242,381897,381552,
        381208,380864,380521,380177,379834,379492,379149,378807,
        378466,378124,377783,377442,377102,376762,376422,376082,
        375743,375404,375065,374727,374389,374051,373714,373377,
        373040,372703,372367,372031,371695,371360,371025,370690,
        370356,370022,369688,369355,369021,368688,368356,368023,
        367691,367360,367028,366697,366366,366036,365706,365376,
        365046,364717,364388,364059,363731,363403,363075,362747,
        362420,362093,361766,361440,361114,360788,360463,360137,
        359813,359488,359164,358840,358516,358193,357869,357547,
        357224,356902,356580,356258,355937,355616,355295,354974,
        354654,354334,354014,353695,353376,353057,352739,352420,
        352103,351785,351468,351150,350834,350517,350201,349885,
        349569,349254,348939,348624,348310,347995,347682,347368,
        347055,346741,346429,346116,345804,345492,345180,344869,
        344558,344247,343936,343626,343316,343006,342697,342388,
        342079,341770,341462,341154,340846,340539,340231,339924,
        339618,339311,339005,338700,338394,338089,337784,337479,
        337175,336870,336566,336263,335959,335656,335354,335051,
        334749,334447,334145,333844,333542,333242,332941,332641,
        332341,332041,331741,331442,331143,330844,330546,330247,
        329950,329652,329355,329057,328761,328464,328168,327872,
        327576,327280,326985,326690,326395,326101,325807,325513,
        325219,324926,324633,324340,324047,323755,323463,323171,
        322879,322588,322297,322006,321716,321426,321136,320846,
        320557,320267,319978,319690,319401,319113,318825,318538,
        318250,317963,317676,317390,317103,316817,316532,316246,
        315961,315676,315391,315106,314822,314538,314254,313971,
        313688,313405,313122,312839,312557,312275,311994,311712,
        311431,311150,310869,310589,310309,310029,309749,309470,
        309190,308911,308633,308354,308076,307798,307521,307243,
        306966,306689,306412,306136,305860,305584,305308,305033,
        304758,304483,304208,303934,303659,303385,303112,302838,
        302565,302292,302019,301747,301475,301203,300931,300660,
        300388,300117,299847,299576,299306,299036,298766,298497,
        298227,297958,297689,297421,297153,296884,296617,296349,
        296082,295815,295548,295281,295015,294749,294483,294217,
        293952,293686,293421,293157,292892,292628,292364,292100,
        291837,291574,291311,291048,290785,290523,290261,289999,
        289737,289476,289215,288954,288693,288433,288173,287913,
        287653,287393,287134,286875,286616,286358,286099,285841,
        285583,285326,285068,284811,284554,284298,284041,283785,
        283529,283273,283017,282762,282507,282252,281998,281743,
        281489,281235,280981,280728,280475,280222,279969,279716,
        279464,279212,278960,278708,278457,278206,277955,277704,
        277453,277203,276953,276703,276453,276204,275955,275706,
        275457,275209,274960,274712,274465,274217,273970,273722,
        273476,273229,272982,272736,272490,272244,271999,271753,
        271508,271263,271018,270774,270530,270286,270042,269798,
        269555,269312,269069,268826,268583,268341,268099,267857);
{$endif}

{$ifndef android}
{$ifdef windows}
function SemaphoreInit:pointer;
begin
 getmem(result,sizeof(ptruint));
 ptruint(result^):=CreateEvent(nil,false,false,nil);
end;

procedure SemaphoreWait(const Semaphore:pointer);
begin
 WaitForSingleObject(ptruint(Semaphore^),{$ifdef wince}ptruint(ptrint(-1)){$else}Windows.INFINITE{$endif});
end;

procedure SemaphorePost(const Semaphore:pointer);
begin
 SetEvent(ptruint(Semaphore^));
end;

procedure SemaphoreDestroy(const Semaphore:pointer);
begin
 CloseHandle(ptruint(Semaphore^));
 freemem(Semaphore);
end;
{$else}
function SemaphoreInit:pointer;
begin
 getmem(result,sizeof(TFilDes));
 fppipe(PFilDes(result)^);
end;

procedure SemaphoreWait(const Semaphore:pointer);
var b:byte;
begin
 fpread(PFilDes(Semaphore)^[0],b,1);
end;

procedure SemaphorePost(const Semaphore:pointer);
var b:byte;
begin
 b:=0;
 fpwrite(PFilDes(Semaphore)^[1],b,1);
end;

procedure SemaphoreDestroy(const Semaphore:pointer);
begin
 fpclose(PFilDes(Semaphore)^[0]);
 fpclose(PFilDes(Semaphore)^[1]);
 freemem(Semaphore);
end;
{$endif}
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

function sar(Value,Shift:integer):integer;
{$ifdef cpu386}
{$ifdef fpc} assembler; inline;
asm
 mov ecx,edx
 sar eax,cl
end ['eax','edx','ecx'];
{$else} assembler; register;
asm
 mov ecx,edx
 sar eax,cl
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

function SARPosition(Value:integer):integer;
{$ifdef cpu386}
{$ifdef fpc} assembler; inline;
asm
 sar eax,BeRoXMPositionShift
end ['eax'];
{$else} assembler; register;
asm
 sar eax,BeRoXMPositionShift
end;
{$endif}
{$else}
{$ifdef cpuarm} assembler; inline;
asm
 mov r0,r0,asr #beRoXMPositionShift
end ['r0'];
{$else}{$ifdef fpc}inline;{$endif}
begin
{$ifdef HasSAR}
 result:=SARLongint(Value,BeRoXMPositionShift);
{$else}
 result:=sar(Value,BeRoXMPositionShift);
{$endif}
end;
{$endif}
{$endif}

function SARFilter(Value:integer):integer;
{$ifdef cpu386}
{$ifdef fpc} assembler; inline;
asm
 sar eax,BeRoXMFilterBits
end ['eax'];
{$else} assembler; register;
asm
 sar eax,BeRoXMFilterBits
end;
{$endif}
{$else}
{$ifdef cpuarm} assembler; inline;
asm
 mov r0,r0,asr #beRoXMFilterBits
end ['r0'];
{$else}{$ifdef fpc}inline;{$endif}
begin
{$ifdef HasSAR}
 result:=SARLongint(Value,BeRoXMFilterBits);
{$else}
 result:=sar(Value,BeRoXMFilterBits);
{$endif}
end;
{$endif}
{$endif}

function SARRamp(Value:integer):integer;
{$ifdef cpu386}
{$ifdef fpc} assembler; inline;
asm
 sar eax,BeRoXMRampBits
end ['eax'];
{$else} assembler; register;
asm
 sar eax,BeRoXMRampBits
end;
{$endif}
{$else}
{$ifdef cpuarm} assembler; inline;
asm
 mov r0,r0,asr #beRoXMRampBits
end ['r0'];
{$else}{$ifdef fpc}inline;{$endif}
begin
{$ifdef HasSAR}
 result:=SARLongint(Value,BeRoXMRampBits);
{$else}
 result:=sar(Value,BeRoXMRampBits);
{$endif}
end;
{$endif}
{$endif}

function SARLinear(Value:integer):integer;
{$ifdef cpu386}
{$ifdef fpc} assembler; inline;
asm
 sar eax,BeRoXMLinearShift
end ['eax'];
{$else} assembler; register;
asm
 sar eax,BeRoXMLinearShift
end;
{$endif}
{$else}
{$ifdef cpuarm} assembler; inline;
asm
 mov r0,r0,asr #beRoXMLinearShift
end ['r0'];
{$else}{$ifdef fpc}inline;{$endif}
begin
{$ifdef HasSAR}
 result:=SARLongint(Value,BeRoXMLinearShift);
{$else}
 result:=sar(Value,BeRoXMLinearShift);
{$endif}
end;
{$endif}
{$endif}

function SARCubicSplineValue(Value:integer):integer;
{$ifdef cpu386}
{$ifdef fpc} assembler; inline;
asm
 sar eax,BeRoXMCubicSplineValueBits
end ['eax'];
{$else} assembler; register;
asm
 sar eax,BeRoXMCubicSplineValueBits
end;
{$endif}
{$else}
{$ifdef cpuarm} assembler; inline;
asm
 mov r0,r0,asr #beRoXMCubicSplineValueBits
end ['r0'];
{$else}{$ifdef fpc}inline;{$endif}
begin
{$ifdef HasSAR}
 result:=SARLongint(Value,BeRoXMCubicSplineValueBits);
{$else}
 result:=sar(Value,BeRoXMCubicSplineValueBits);
{$endif}
end;
{$endif}
{$endif}

function SARWindowedFIRValue(Value:integer):integer;
{$ifdef cpu386}
{$ifdef fpc} assembler; inline;
asm
 sar eax,BeRoXMWindowedFIRValueBits
end ['eax'];
{$else} assembler; register;
asm
 sar eax,BeRoXMWindowedFIRValueBits
end;
{$endif}
{$else}
{$ifdef cpuarm} assembler; inline;
asm
 mov r0,r0,asr #beRoXMWindowedFIRValueBits
end ['r0'];
{$else}{$ifdef fpc}inline;{$endif}
begin
{$ifdef HasSAR}
 result:=SARLongint(Value,BeRoXMWindowedFIRValueBits);
{$else}
 result:=sar(Value,BeRoXMWindowedFIRValueBits);
{$endif}
end;
{$endif}
{$endif}

function SAROut(Value:integer):integer;
{$ifdef cpu386}
{$ifdef fpc} assembler; inline;
asm
 sar eax,BeRoXMOutBits
end ['eax'];
{$else} assembler; register;
asm
 sar eax,BeRoXMOutBits
end;
{$endif}
{$else}
{$ifdef cpuarm} assembler; inline;
asm
 mov r0,r0,asr #beRoXMOutBits
end ['r0'];
{$else}{$ifdef fpc}inline;{$endif}
begin
{$ifdef HasSAR}
 result:=SARLongint(Value,BeRoXMOutBits);
{$else}
 result:=sar(Value,BeRoXMOutBits);
{$endif}
end;
{$endif}
{$endif}

function SAR1(Value:integer):integer;
{$ifdef cpu386}
{$ifdef fpc} assembler; inline;
asm
 sar eax,1
end ['eax'];
{$else} assembler; register;
asm
 sar eax,1
end;
{$endif}
{$else}
{$ifdef cpuarm} assembler; inline;
asm
 mov r0,r0,asr #1
end ['r0'];
{$else}{$ifdef fpc}inline;{$endif}
begin
{$ifdef HasSAR}
 result:=SARLongint(Value,1);
{$else}
 result:=sar(Value,1);
{$endif}
end;
{$endif}
{$endif}

{$ifdef cpuarm}
procedure FillChar(const P;Count:integer;C:char);
var PB:pchar;
    I:integer;
begin
 PB:=@P;
 for I:=1 to Count shr 2 do begin
  PB^:=C;
  inc(PB);
  PB^:=C;
  inc(PB);
  PB^:=C;
  inc(PB);
  PB^:=C;
  inc(PB);
 end;
 for I:=1 to Count and 3 do begin
  PB^:=C;
  inc(PB);
 end;
end;
{$endif}

function Hex2Str(Value:byte):string;
const Hex:array[0..15] of char='0123456789ABCDEF';
begin
 result:=Hex[(Value shr 4) and $f]+Hex[Value and $f];
end;

function trim(const S:string):string;
var StartPosition,TheLength:integer;
begin
 TheLength:=length(S);
 if TheLength>0 then begin
  while (TheLength>0) and (S[TheLength] in [#0..#32]) do begin
   dec(TheLength);
  end;
  StartPosition:=1;
  while (StartPosition<=TheLength) and (S[StartPosition] in [#0..#32]) do begin
   inc(StartPosition);
  end;
  result:=copy(S,StartPosition,TheLength-StartPosition+1);
 end else begin
  result:='';
 end;
end;

function pow(Number,Exponent:single):single;
begin
 result:=exp(Exponent*ln(Number));
end;

function log2(Value:single):single;
begin
 if Value<>0 then begin
  result:=ln(Value)/ln(2);
 end else begin
  result:=0;
 end;
end;

procedure CreateRingBuffer(var RingBuffer:TBeRoXMRingBuffer;Size:longword);
begin
 FillChar(RingBuffer,sizeof(TBeRoXMRingBuffer),#0);
 RingBuffer.Size:=Size;
 getmem(RingBuffer.Data,sizeof(byte)*Size);
 FillChar(RingBuffer.Data^,sizeof(byte)*Size,#0);
end;

procedure DestroyRingBuffer(var RingBuffer:TBeRoXMRingBuffer);
begin
 if assigned(RingBuffer.Data) then begin
  freemem(RingBuffer.Data);
  RingBuffer.Data:=nil;
 end;
 FillChar(RingBuffer,sizeof(TBeRoXMRingBuffer),#0);
end;

function RingBufferUsed(var RingBuffer:TBeRoXMRingBuffer):longword;
var WritePos,ReadPos:longword;
begin
 WritePos:=RingBuffer.WritePos;
 ReadPos:=RingBuffer.ReadPos;
 if WritePos>=ReadPos then begin
  result:=WritePos-ReadPos;
 end else begin
  result:=(RingBuffer.Size-ReadPos)+WritePos;
 end;
 while result>=RingBuffer.Size do begin
  dec(result,RingBuffer.Size);
 end;
end;

function RingBufferSpace(var RingBuffer:TBeRoXMRingBuffer):longword;
begin
 result:=RingBuffer.Size-RingBufferUsed(RingBuffer);
 if result>0 then begin
  dec(result);
 end;
end;

function ReadFromRingBuffer(var RingBuffer:TBeRoXMRingBuffer;Buffer:pointer;Bytes:longword):longword;
var Total,ReadPos,ToRead,Len:longword;
    p:pchar;
begin
 p:=pointer(Buffer);
 Len:=Bytes;
 Total:=RingBufferUsed(RingBuffer);
 if Len>Total then begin
  Len:=Total;
 end else begin
  Total:=Len;
 end;
 ReadPos:=RingBuffer.ReadPos;
 while Len>0 do begin
  if (ReadPos+Len)>RingBuffer.Size then begin
   ToRead:=RingBuffer.Size-ReadPos;
   move(RingBuffer.Data^[ReadPos],p^,ToRead);
   inc(p,ToRead);
   dec(Len,ToRead);
   ReadPos:=0;
  end else begin
   move(RingBuffer.Data^[ReadPos],p^,Len);
   inc(ReadPos,Len);
   break;
  end;
 end;
 while ReadPos>=RingBuffer.Size do begin
  dec(ReadPos,RingBuffer.Size);
 end;
{$ifdef fpc}
  InterLockedExchange(longint(RingBuffer.ReadPos),longint(ReadPos));
{$else}
{$ifdef cpu386}
 asm
  // SMP-safe, thread-safe and lock-free RingBuffer.ReadPos:=ReadPos;
  push eax
  push ecx
   mov eax,dword ptr ReadPos
   mov ecx,dword ptr RingBuffer
   lea ecx,dword ptr [ecx+TBeRoXMRingBuffer.ReadPos]
   lock xchg dword ptr [ecx],eax
  pop ecx
  pop eax
 end;
{$else}
 RingBuffer.ReadPos:=ReadPos;
{$endif}
{$endif}
 result:=Total;
end;

function WriteToRingBuffer(var RingBuffer:TBeRoXMRingBuffer;Buffer:pointer;Bytes:longword):longword;
var Total,WritePos,ToWrite,Len:longword;
    p:pchar;
begin
 Total:=RingBufferSpace(RingBuffer);
 Len:=Bytes;
 if Total>=Len then begin
  p:=pointer(Buffer);
  if Len>Total then begin
   Len:=Total;
  end;
  WritePos:=RingBuffer.WritePos;
  while Len>0 do begin
   if (WritePos+Len)>RingBuffer.Size then begin
    ToWrite:=RingBuffer.Size-WritePos;
    move(p^,RingBuffer.Data^[WritePos],ToWrite);
    inc(p,ToWrite);
    dec(Len,ToWrite);
    WritePos:=0;
   end else begin
    move(p^,RingBuffer.Data^[WritePos],Len);
    inc(WritePos,Len);
    break;
   end;
  end;
  while WritePos>=RingBuffer.Size do begin
   dec(WritePos,RingBuffer.Size);
  end;
{$ifdef fpc}
  InterLockedExchange(longint(RingBuffer.WritePos),longint(WritePos));
{$else}
{$ifdef cpu386}
  asm
   // SMP-safe, thread-safe and lock-free RingBuffer.WritePos:=WritePos;
   push eax
    mov eax,dword ptr WritePos
    mov ecx,dword ptr RingBuffer
    lea ecx,dword ptr [ecx+TBeRoXMRingBuffer.WritePos]
    lock xchg dword ptr [ecx],eax
   pop eax
  end;
{$else}
 RingBuffer.WritePos:=WritePos;
{$endif}
{$endif}
  result:=Total;
 end else begin
  result:=0;
 end;
end;

procedure ResetRingBuffer(var RingBuffer:TBeRoXMRingBuffer);
begin
 RingBuffer.ReadPos:=0;
 RingBuffer.WritePos:=0;
end;

type pbyte=^byte;

const MemoryDelta=1 shl 16;
      MemoryDeltaMask=MemoryDelta-1;

constructor TBeRoXMStream.Create;
begin
 inherited Create;
 fData:=nil;
 ReallocMem(fData,0);
 fPosition:=0;
 fSize:=0;
 fInMemorySize:=0;
 ResetBits;
end;

destructor TBeRoXMStream.Destroy;
begin
 ReallocMem(fData,0);
 fPosition:=0;
 fSize:=0;
 fInMemorySize:=0;
 inherited Destroy;
end;

function TBeRoXMStream.Assign(Src:TBeRoXMStream):integer;
var Remain,Count:integer;
    Buf:TBeRoXMStreamBuffer;
begin
 Clear;
 result:=0;
 Remain:=Src.Size;
 if (Seek(0)<>0) or (Src.Seek(0)<>0) then begin
  exit;
 end;
 while Remain>=sizeof(TBeRoXMStreamBuffer) do begin
  Count:=Src.Read(Buf,sizeof(TBeRoXMStreamBuffer));
  Write(Buf,Count);
  inc(result,Count);
  dec(Remain,sizeof(TBeRoXMStreamBuffer));
 end;
 Count:=Src.Read(Buf,Remain);
 Write(Buf,Count);
 inc(result,Count);
end;

function TBeRoXMStream.Append(Src:TBeRoXMStream):integer;
var Remain,Count:integer;
    Buf:TBeRoXMStreamBuffer;
begin
 result:=0;
 Remain:=Src.Size;
 if Src.Seek(0)<>0 then begin
  exit;
 end;
 while Remain>=sizeof(TBeRoXMStreamBuffer) do begin
  Count:=Src.Read(Buf,sizeof(TBeRoXMStreamBuffer));
  Write(Buf,Count);
  inc(result,Count);
  dec(Remain,sizeof(TBeRoXMStreamBuffer));
 end;
 Count:=Src.Read(Buf,Remain);
 Write(Buf,Count);
 inc(result,Count);
end;

function TBeRoXMStream.AppendFrom(Src:TBeRoXMStream;Counter:integer):integer;
var Remain,Count:integer;
    Buf:TBeRoXMStreamBuffer;
begin
 result:=0;
 Remain:=Counter;
 while Remain>=sizeof(TBeRoXMStreamBuffer) do begin
  Count:=Src.Read(Buf,sizeof(TBeRoXMStreamBuffer));
  Write(Buf,Count);
  inc(result,Count);
  dec(Remain,sizeof(TBeRoXMStreamBuffer));
 end;
 Count:=Src.Read(Buf,Remain);
 Write(Buf,Count);
 inc(result,Count);
end;

procedure TBeRoXMStream.Clear;
begin
 ReallocMem(fData,0);
 fPosition:=0;
 fSize:=0;
 fInMemorySize:=0;
end;

procedure TBeRoXMStream.Realloc(NewInMemorySize:integer);
var OldInMemorySize,Count:integer;
begin
 if NewInMemorySize>0 then begin
  NewInMemorySize:=(NewInMemorySize+MemoryDeltaMask) and not MemoryDeltaMask;
 end;
 if fInMemorySize<>NewInMemorySize then begin
  OldInMemorySize:=fInMemorySize;
  fInMemorySize:=NewInMemorySize;
  ReallocMem(fData,fInMemorySize);
  Count:=NewInMemorySize-OldInMemorySize;
  if Count>0 then begin
   FillChar(fData^[OldInMemorySize],Count,#0);
  end;
 end;
end;

procedure TBeRoXMStream.Resize(NewSize:integer);
begin
 fSize:=NewSize;
 if fPosition>fSize then fPosition:=fSize;
 Realloc(fSize);
end;

function TBeRoXMStream.Read(var Buf;Count:integer):integer;
begin
 if (fPosition>=0) and (Count>0) then begin
  result:=fSize-fPosition;
  if result>0 then begin
   if result>Count then result:=Count;
   MOVE(fData^[fPosition],Buf,result);
   inc(fPosition,result);
   exit;
  end;
 end;
 result:=0;
end;

function TBeRoXMStream.ReadAt(Position:integer;var Buf;Count:integer):integer;
begin
 if Seek(Position)=Position then begin
  result:=Read(Buf,Count);
 end else begin
  result:=0;
 end;
end;

function TBeRoXMStream.Write(const Buf;Count:integer):integer;
var EndPosition:integer;
begin
if (fPosition>=0) and (Count>0) then begin
  EndPosition:=fPosition+Count;
  if EndPosition>fSize then Resize(EndPosition);
  MOVE(Buf,fData^[fPosition],Count);
  fPosition:=EndPosition;
  result:=Count;
  exit;
 end;
 result:=0;
end;

function TBeRoXMStream.SeekEx(APosition:integer):integer;
var AltePos,RemainSize:integer;
begin
 fPosition:=APosition;
 if fPosition<0 then fPosition:=0;
 if fPosition>fSize then begin
  AltePos:=fSize;
  RemainSize:=fPosition-fSize;
  if RemainSize>0 then begin
   Resize(fSize+RemainSize);
   FillChar(fData^[AltePos],RemainSize,#0);
  end;
  result:=fPosition;
 end else begin
  result:=fPosition;
 end;
end;

function TBeRoXMStream.Seek(APosition:integer):integer;
begin
 result:=SeekEx(APosition);
end;

function TBeRoXMStream.Seek(APosition,Origin:integer):integer;
begin
 case Origin of
  bsoFromBeginning:result:=SeekEx(APosition);
  bsoFromCurrent:result:=SeekEx(Position+APosition);
  bsoFromEnd:result:=SeekEx(Size-APosition);
  else result:=SeekEx(APosition);
 end;
end;

function TBeRoXMStream.Position:integer;
begin
 result:=fPosition;
end;

function TBeRoXMStream.Size:integer;
begin
 result:=fSize;
end;

procedure TBeRoXMStream.SetSize(NewSize:integer);
begin
 fSize:=NewSize;
 if fPosition>fSize then fPosition:=fSize;
 ReallocMem(fData,fSize);
end;

function TBeRoXMStream.ReadByte:byte;
var B:byte;
begin
 if Read(B,1)<>1 then begin
  result:=0;
 end else begin
  result:=B;
 end;
end;

function TBeRoXMStream.ReadWord:word;
begin
 result:=ReadByte or (ReadByte shl 8);
end;

function TBeRoXMStream.ReadDWord:longword;
begin
 result:=ReadWord or (ReadWord shl 16);
end;

function TBeRoXMStream.ReadLine:string;
var C:char;
begin
 result:='';
 while Position<Size do begin
  Read(C,1);
  case C of
   #10,#13:begin
    while (Position<Size) and (Bytes[Position] in [10,13]) do begin
     Read(C,1);
    end;
    break;
   end;
   else result:=result+C;
  end;
 end;
end;

function TBeRoXMStream.ReadString:string;
var L:longword;
begin
 L:=ReadDWord;
 setlength(result,L);
 if L>0 then Read(result[1],L);
end;

function TBeRoXMStream.ReadWideString:widestring;
var L:longword;
begin
 L:=ReadDWord;
 setlength(result,L);
 if L>0 then Read(result[1],L*sizeof(widechar));
end;

procedure TBeRoXMStream.WriteByte(Value:byte);
begin
 Write(Value,sizeof(byte));
end;

function TBeRoXMStream.WriteByteCount(Value:byte;Count:integer):integer;
var Counter:integer;
begin
 result:=0;
 for Counter:=1 to Count do inc(result,Write(Value,sizeof(byte)));
end;

procedure TBeRoXMStream.WriteWord(Value:word);
begin
 Write(Value,sizeof(word));
end;

procedure TBeRoXMStream.WriteDWord(Value:longword);
begin
 Write(Value,sizeof(longword));
end;

procedure TBeRoXMStream.WriteShortInt(Value:shortint);
begin
 Write(Value,sizeof(shortint));
end;

procedure TBeRoXMStream.WriteSmallInt(Value:smallint);
begin
 Write(Value,sizeof(smallint));
end;

procedure TBeRoXMStream.WriteLongInt(Value:longint);
begin
 Write(Value,sizeof(longint));
end;

procedure TBeRoXMStream.WriteInt64(Value:int64);
begin
 Write(Value,sizeof(int64));
end;

procedure TBeRoXMStream.WriteBoolean(Value:boolean);
begin
 if Value then begin
  WriteByte(1);
 end else begin
  WriteByte(0);
 end;
end;

procedure TBeRoXMStream.WriteLine(Line:string);
const CRLF:array[1..2] of char=#13#10;
begin
 if length(Line)>0 then Write(Line[1],length(Line));
 Write(CRLF,2);
end;

procedure TBeRoXMStream.WriteString(S:string);
var L:longword;
begin
 L:=length(S);
 if L>0 then Write(S[1],L);
end;

procedure TBeRoXMStream.WriteDataString(S:string);
var L:longword;
begin
 L:=length(S);
 WriteDWord(L);
 if L>0 then Write(S[1],L);
end;

procedure TBeRoXMStream.WriteDataWideString(S:widestring);
var L:longword;
begin
 L:=length(S);
 WriteDWord(L);
 if L>0 then Write(S[1],L*sizeof(widechar));
end;

procedure TBeRoXMStream.ResetBits;
begin
 fBitBuffer:=0;
 fBitBufferSize:=0;
end;

function TBeRoXMStream.ReadBit:boolean;
begin
 result:=(ReadBits(1)<>0);
end;

function TBeRoXMStream.ReadBits(BitsCount:byte):longword;
begin
 while fBitBufferSize<BitsCount do begin
  fBitBuffer:=(fBitBuffer shl 8) or ReadByte;
  inc(fBitBufferSize,8);
 end;
 result:=(fBitBuffer shr (fBitBufferSize-BitsCount)) and ((1 shl BitsCount)-1);
 dec(fBitBufferSize,BitsCount);
end;

function TBeRoXMStream.ReadBitsSigned(BitsCount:byte):longint;
begin
 result:=0;
 if BitsCount>1 then begin
  if ReadBits(1)<>0 then begin
   result:=-ReadBits(BitsCount-1);
  end else begin
   result:=ReadBits(BitsCount-1);
  end;
 end;
end;

procedure TBeRoXMStream.WriteBit(Value:boolean);
begin
 if Value then begin
  WriteBits(1,1);
 end else begin
  WriteBits(0,1);
 end;
end;

procedure TBeRoXMStream.WriteBits(Value:longword;BitsCount:byte);
begin
 fBitBuffer:=(fBitBuffer shl BitsCount) or Value;
 inc(fBitBufferSize,BitsCount);
 while fBitBufferSize>=8 do begin
  WriteByte((fBitBuffer shr (fBitBufferSize-8)) and $ff);
  dec(fBitBufferSize,8);
 end;
end;

procedure TBeRoXMStream.WriteBitsSigned(Value:longint;BitsCount:byte);
begin
 if BitsCount>1 then begin
  if Value<0 then begin
   WriteBits(1,1);
   WriteBits(longword(0-Value),BitsCount-1);
  end else begin
   WriteBits(0,1);
   WriteBits(longword(Value),BitsCount-1);
  end;
 end;
end;

procedure TBeRoXMStream.FlushBits;
begin
 if fBitBufferSize>0 then begin
  WriteByte(fBitBuffer shl (8-fBitBufferSize));
 end;
 fBitBuffer:=0;
 fBitBufferSize:=0;
end;

function TBeRoXMStream.GetString:string;
begin
 Seek(0);
 if Size>0 then begin
  setlength(result,Size);
  Read(result[1],Size);
 end else begin
  result:='';
 end;
end;

procedure TBeRoXMStream.setstring(Value:string);
begin
 Clear;
 if length(Value)>0 then begin
  Write(Value[1],length(Value));
 end;
end;

function TBeRoXMStream.GetByte(BytePosition:integer):byte;
var AltePosition:integer;
begin
 AltePosition:=Position;
 Seek(BytePosition);
 Read(result,sizeof(byte));
 Seek(AltePosition);
end;

procedure TBeRoXMStream.SetByte(BytePosition:integer;Value:byte);
var AltePosition:integer;
begin
 AltePosition:=Position;
 Seek(BytePosition);
 Write(Value,sizeof(byte));
 Seek(AltePosition);
end;

constructor TBeRoXMFileStream.Create(FileName:string);
var Alt:byte;
begin
 inherited Create;
 Alt:=FileMode;
 FileMode:=0;
 AssignFile(fFile,FileName);
 {$I-}Reset(fFile,1);{$I+}
 FileMode:=Alt;
 if IOResult<>0 then {$I-}Rewrite(fFile,1);{$I+}
 if IOResult<>0 then begin
 end;
end;

constructor TBeRoXMFileStream.CreateNew(FileName:string);
var Alt:byte;
begin
 inherited Create;
 Alt:=FileMode;
 FileMode:=2;
 AssignFile(fFile,FileName);
 {$I-}Rewrite(fFile,1);{$I+}
 FileMode:=Alt;
 if IOResult<>0 then begin
 end;
end;

destructor TBeRoXMFileStream.Destroy;
begin
 {$I-}CloseFile(fFile);{$I+}
 if IOResult<>0 then begin
 end;
 inherited Destroy;
end;

function TBeRoXMFileStream.Read(var Buf;Count:integer):integer;
var I:integer;
begin
 {$I-}BlockRead(fFile,Buf,Count,I);{$I+}
 if IOResult<>0 then begin
  result:=0;
  exit;
 end;
 {$I-}fPosition:=FilePos(fFile);{$I+}
 if IOResult<>0 then begin
  result:=0;
  exit;
 end;
 result:=I;
end;

function TBeRoXMFileStream.Write(const Buf;Count:integer):integer;
var I:integer;
begin
 {$I-}BlockWrite(fFile,Buf,Count,I);{$I+}
 if IOResult<>0 then begin
  result:=0;
  exit;
 end;
 {$I-}fPosition:=FilePos(fFile);{$I+}
 if IOResult<>0 then begin
  result:=0;
  exit
 end;
 result:=I;
end;

function TBeRoXMFileStream.SeekEx(APosition:integer):integer;
begin
 if APosition<=Size then begin
  {$I-}System.Seek(fFile,APosition);{$I+}
  if IOResult<>0 then begin
   result:=0;
   exit;
  end;
 end;
 {$I-}result:=FilePos(fFile);{$I+}
 if IOResult<>0 then begin
  result:=0;
 end;
end;

function TBeRoXMFileStream.Position:integer;
begin
 {$I-}result:=FilePos(fFile);{$I+}
 if IOResult<>0 then begin
  result:=0;
 end;
end;

function TBeRoXMFileStream.Size:integer;
begin
 {$I-}result:=FileSize(fFile);{$I+}
 if IOResult<>0 then begin
  result:=0;
 end;
end;

{$ifndef android}
{$ifdef windows}
procedure BeRoXMThreadProc(BeRoXM:TBeRoXM);
{$else}
function BeRoXMThreadProc(BeRoXM:TBeRoXM):PtrInt;
{$endif}
begin
 while not BeRoXM.ThreadTerminated do begin
  if BeRoXM.ThreadIsSuspended<>BeRoXM.ThreadSuspended then begin
   if BeRoXM.ThreadSuspended then begin
    BeRoXM.ThreadIsSuspended:=true;
    SemaphorePost(BeRoXM.ThreadSuspendSemaphore);
    SemaphoreWait(BeRoXM.ThreadResumeSemaphore);
   end;
   BeRoXM.ThreadIsSuspended:=false;
   SemaphorePost(BeRoXM.ThreadActiveSemaphore);
   if BeRoXM.ThreadTerminated then begin
    break;
   end;
  end;
  BeRoXM.Enter;
  BeRoXM.Poll;
  BeRoXM.Leave;
  sleep(BeRoXM.SleepTime);
 end;
 EndThread(0);
end;
{$endif}

constructor TBeRoXMAudioProcessChainItem.Create(TheChain:TBeRoXMAudioProcessChain);
begin
 inherited Create;
 Chain:=TheChain;
 Previous:=nil;
 Next:=nil;
end;

destructor TBeRoXMAudioProcessChainItem.Destroy;
begin
 inherited Destroy;
end;

procedure TBeRoXMAudioProcessChainItem.Reset;
begin
end;

procedure TBeRoXMAudioProcessChainItem.Process;
begin
end;

function TBeRoXMAudioProcessChainItem.LoadFromStream(Stream:TBeRoXMStream):boolean;
begin
 result:=false;
end;

function TBeRoXMAudioProcessChainItem.SaveToStream(Stream:TBeRoXMStream):boolean;
begin
 result:=false;
end;

constructor TBeRoXMAudioProcessChainItemUnknown.Create(TheChain:TBeRoXMAudioProcessChain);
begin
 inherited Create(TheChain);
 FillChar(ChunkSignature,sizeof(TBeRoXMChunkSignature),#0);
 Data:=TBeRoXMStream.Create;
end;

destructor TBeRoXMAudioProcessChainItemUnknown.Destroy;
begin
 Data.Destroy;
 inherited DEstroy;
end;

function TBeRoXMAudioProcessChainItemUnknown.LoadFromStream(Stream:TBeRoXMStream):boolean;
begin
 Data.Assign(Stream);
 result:=true;
end;

function TBeRoXMAudioProcessChainItemUnknown.SaveToStream(Stream:TBeRoXMStream):boolean;
begin
 Stream.Assign(Data);
 result:=true;
end;

constructor TBeRoXMAudioProcessChain.Create(TheModule:TBeRoXMModule;TheBufferSamples:integer;TheBufferChannels:integer);
begin
 inherited Create;
 Module:=TheModule;
 BufferSamples:=TheBufferSamples;
 BufferChannels:=TheBufferChannels;
 BufferSize:=BufferSamples*BufferChannels*sizeof(longint);
 getmem(Buffer,BufferSize);
 First:=nil;
 Last:=nil;
 Clear;
end;

destructor TBeRoXMAudioProcessChain.Destroy;
begin
 Clear;
 freemem(Buffer);
 inherited Destroy;
end;

procedure TBeRoXMAudioProcessChain.Clear;
var Item,NextItem:TBeRoXMAudioProcessChainItem;
begin
 Item:=First;
 while assigned(Item) do begin
  NextItem:=Item.Next;
  Item.Destroy;
  Item:=NextItem;
 end;

 First:=nil;
 Last:=nil;
 Active:=false;
 Mix:=true;
 Name:='';
 Reset;
end;

procedure TBeRoXMAudioProcessChain.Reset;
var Item:TBeRoXMAudioProcessChainItem;
begin
 Item:=First;
 while assigned(Item) do begin
  Item.Reset;
  Item:=Item.Next;
 end;

 LastLeft:=0;
 LastRight:=0;
end;

procedure TBeRoXMAudioProcessChain.ResetBuffer(Samples:integer);
begin
 FillChar(Buffer^,Samples*BufferChannels*sizeof(longint),#0);
end;

procedure TBeRoXMAudioProcessChain.ProcessClickRemoval(LengthCounter:integer);
var Counter,LocalLastLeft,LocalLastRight:integer;
    Buf:plongint;
begin
 if Module.Owner.OutputChannels=2 then begin
  LocalLastLeft:=LastLeft;
  LocalLastRight:=LastRight;
  if (LocalLastLeft<>0) or (LocalLastRight<>0) then begin
   Buf:=pointer(@Buffer^[0]);
   for Counter:=0 to LengthCounter-1 do begin
{$ifdef HasSAR}
    dec(LocalLastLeft,SARLongint(LocalLastLeft+(SARLongint(-LocalLastLeft,31) and $ff),8));
    dec(LocalLastRight,SARLongint(LocalLastRight+(SARLongint(-LocalLastRight,31) and $ff),8));
{$else}
{$ifdef UseSAR}
    dec(LocalLastLeft,sar(LocalLastLeft+(sar(-LocalLastLeft,31) and $ff),8));
    dec(LocalLastRight,sar(LocalLastRight+(sar(-LocalLastRight,31) and $ff),8));
{$else}
    dec(LocalLastLeft,(LocalLastLeft+(((-LocalLastLeft) div (1 shl 31)) and $ff)) div 256);
    dec(LocalLastRight,(LocalLastRight+(((-LocalLastRight) div (1 shl 31)) and $ff)) div 256);
{$endif}
{$endif}
    Buf^:=Buf^+LocalLastLeft;
    inc(Buf);
    Buf^:=Buf^+LocalLastRight;
    inc(Buf);
   end;
   LastLeft:=LocalLastLeft;
   LastRight:=LocalLastRight;
  end;
 end else begin
  LocalLastLeft:=LastLeft;
  if LocalLastLeft<>0 then begin
   Buf:=pointer(@Buffer^[0]);
   for Counter:=0 to LengthCounter-1 do begin
{$ifdef HasSAR}
    dec(LocalLastLeft,SARLongint(LocalLastLeft+(sar(-LocalLastLeft,31) and $ff),8));
{$else}
{$ifdef UseSAR}
    dec(LocalLastLeft,sar(LocalLastLeft+(sar(-LocalLastLeft,31) and $ff),8));
{$else}
    dec(LocalLastLeft,(LocalLastLeft+(((-LocalLastLeft) div (1 shl 31)) and $ff)) div 256);
{$endif}
{$endif}
    Buf^:=Buf^+LocalLastLeft;
    inc(Buf);
   end;
   LastLeft:=LocalLastLeft;
  end;
 end;
end;

procedure TBeRoXMAudioProcessChain.ProcessBuffer(StartPosition,Samples:integer);
var Counter,TotalSamples:integer;
    SrcBuffer,DestBuffer:plongint;
    Item:TBeRoXMAudioProcessChainItem;
begin
 ProcessClickRemoval(Samples);

 Item:=First;
 while assigned(Item) do begin
  Item.Process;
  Item:=Item.Next;
 end;

 SrcBuffer:=pointer(@Buffer^[0]);
 DestBuffer:=pointer(@Module.Owner.Buffer^[StartPosition*Module.Owner.OutputChannels]);
 TotalSamples:=Samples*BufferChannels;
{$ifdef UnrolledLoops}
 for Counter:=1 to TotalSamples shr 2 do begin
  DestBuffer^:=DestBuffer^+SrcBuffer^;
  inc(SrcBuffer);
  inc(DestBuffer);
  DestBuffer^:=DestBuffer^+SrcBuffer^;
  inc(SrcBuffer);
  inc(DestBuffer);
  DestBuffer^:=DestBuffer^+SrcBuffer^;
  inc(SrcBuffer);
  inc(DestBuffer);
  DestBuffer^:=DestBuffer^+SrcBuffer^;
  inc(SrcBuffer);
  inc(DestBuffer);
 end;
 for Counter:=1 to TotalSamples and 3 do begin
  DestBuffer^:=DestBuffer^+SrcBuffer^;
  inc(SrcBuffer);
  inc(DestBuffer);
 end;
{$else}
 for Counter:=1 to TotalSamples do begin
  DestBuffer^:=DestBuffer^+SrcBuffer^;
  inc(SrcBuffer);
  inc(DestBuffer);
 end;
{$endif}
end;

procedure TBeRoXMAudioProcessChain.Append(Item:TBeRoXMAudioProcessChainItem);
begin
 if assigned(Last) then begin
  Item.Next:=nil;
  Item.Previous:=Last;
  Last.Next:=Item;
  Last:=Item;
 end else begin
  Item.Next:=nil;
  Item.Previous:=nil;
  First:=Item;
  Last:=Item;
 end;
end;

procedure TBeRoXMAudioProcessChain.Prepend(Item:TBeRoXMAudioProcessChainItem);
begin
 if assigned(First) then begin
  Item.Next:=First;
  Item.Previous:=nil;
  First.Previous:=Item;
  First:=Item;
 end else begin
  Item.Next:=nil;
  Item.Previous:=nil;
  First:=Item;
  Last:=Item;
 end;
end;

procedure TBeRoXMAudioProcessChain.InsertAfter(Item,OtherItem:TBeRoXMAudioProcessChainItem);
var a,b,c:TBeRoXMAudioProcessChainItem;
begin
 if assigned(Last) then begin
  a:=OtherItem;
  b:=Item;
  c:=OtherItem.Next;

  a.Next:=b;
  b.Previous:=a;
  b.Next:=c;
  if assigned(c) then begin
   c.Previous:=b;
  end;

  if Last=a then begin
   Last:=b;
  end;
 end else begin
  Item.Next:=nil;
  Item.Previous:=nil;
  First:=Item;
  Last:=Item;
 end;
end;

procedure TBeRoXMAudioProcessChain.InsertBefore(Item,OtherItem:TBeRoXMAudioProcessChainItem);
var a,b,c:TBeRoXMAudioProcessChainItem;
begin
 if assigned(First) then begin
  a:=OtherItem.Previous;
  b:=Item;
  c:=OtherItem;

  if assigned(a) then begin
   a.Next:=b;
  end;
  b.Previous:=a;
  b.Next:=c;
  c.Previous:=b;

  if First=c then begin
   First:=b;
  end;
 end else begin
  Item.Next:=nil;
  Item.Previous:=nil;
  First:=Item;
  Last:=Item;
 end;
end;

procedure TBeRoXMAudioProcessChain.Remove(Item:TBeRoXMAudioProcessChainItem);
begin
 if assigned(Item.Next) then begin
  Item.Next.Previous:=Item.Previous;
 end;
 if assigned(Item.Previous) then begin
  Item.Previous.Next:=Item.Next;
 end;
 if First=Item then begin
  First:=Item.Next;
 end;
 if Last=Item then begin
  Last:=Item.Previous;
 end;
end;

function TBeRoXMAudioProcessChain.LoadFromStream(Stream:TBeRoXMStream):boolean;
var BlockLastPosition,BlockSize,DataBlockSize:longword;
    Signature:TBeRoXMChunkSignature;
    SubStream,DataStream:TBeRoXMStream;
    ItemUnknown:TBeRoXMAudioProcessChainItemUnknown;
begin
 while (Stream.Position+8)<Stream.Size do begin
  BlockLastPosition:=Stream.Position;
  if Stream.Read(Signature,sizeof(TBeRoXMChunkSignature))<>sizeof(TBeRoXMChunkSignature) then begin
   result:=false;
   exit;
  end;
  if Stream.Read(BlockSize,sizeof(longword))<>sizeof(longword) then begin
   result:=false;
   exit;
  end;
  SwapLittleEndianData32(BlockSize);
  SubStream:=TBeRoXMStream.Create;
  if SubStream.AppendFrom(Stream,BlockSize)<>longint(BlockSize) then begin
   result:=false;
   SubStream.Destroy;
   exit;
  end;
  if Signature='DATA' then begin
   if SubStream.Read(Active,sizeof(bytebool))<>sizeof(bytebool) then begin
    result:=false;
    SubStream.Destroy;
    exit;
   end;
   if SubStream.Read(Mix,sizeof(bytebool))<>sizeof(bytebool) then begin
    result:=false;
    SubStream.Destroy;
    exit;
   end;
  end else if Signature='NAME' then begin
   Name:=SubStream.GetString;
  end else if Signature='ITEM' then begin
   if SubStream.Read(Signature,sizeof(TBeRoXMChunkSignature))<>sizeof(TBeRoXMChunkSignature) then begin
    result:=false;
    SubStream.Destroy;
    exit;
   end;
   if SubStream.Read(DataBlockSize,sizeof(longword))<>sizeof(longword) then begin
    result:=false;
    SubStream.Destroy;
    exit;
   end;
   SwapLittleEndianData32(DataBlockSize);
   DataStream:=TBeRoXMStream.Create;
   if DataStream.AppendFrom(SubStream,DataBlockSize)<>longint(DataBlockSize) then begin
    result:=false;
    DataStream.Destroy;
    SubStream.Destroy;
    exit;
   end;
   if Signature='DELA' then begin
   end else if Signature='CHOR' then begin
   end else if Signature='FILT' then begin
   end else if Signature='FLAN' then begin
   end else if Signature='REVE' then begin
   end else begin
    ItemUnknown:=TBeRoXMAudioProcessChainItemUnknown.Create(self);
    Append(ItemUnknown);
    ItemUnknown.ChunkSignature:=Signature;
    ItemUnknown.LoadFromStream(DataStream);
   end;
   DataStream.Destroy;
  end else begin
   SubStream.Destroy;
   break;
  end;
  SubStream.Destroy;
  Stream.Seek(BlockLastPosition+BlockSize+8);
 end;
 result:=true;
end;

function TBeRoXMAudioProcessChain.SaveToStream(Stream:TBeRoXMStream):boolean;
begin
 result:=false;
end;

constructor TBeRoXMSample.Create;
begin
 inherited Create;
 Data:=nil;
 Clear;
end;

destructor TBeRoXMSample.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TBeRoXMSample.Clear;
begin
 if assigned(Data) then begin
  dec(ptruint(Data),(BeRoXMSampleFixUpLength*2)*sizeof(TBeRoXMSampleValue));
  FreeMem(Data);
  Data:=nil;
 end;
 Bits:=8;
 Channels:=1;
 Name:='';
 Loop:=false;
 PingPongLoop:=false;
 LoopStart:=0;
 LoopLength:=0;
 Volume:=64;
 DoPanning:=true;
 Panning:=128;
 RelativeNote:=0;
 FineTune:=0;
end;

procedure TBeRoXMSample.Resize(Samples:integer);
begin
 if Samples=0 then begin
  if assigned(Data) then begin
   dec(ptruint(Data),(BeRoXMSampleFixUpLength*2)*sizeof(TBeRoXMSampleValue));
   FreeMem(Data);
  end;
  Data:=nil;
 end else begin
  if assigned(Data) then begin
   dec(ptruint(Data),(BeRoXMSampleFixUpLength*2)*sizeof(TBeRoXMSampleValue));
   ReallocMem(Data,(Samples+(BeRoXMSampleFixUpLength*4))*sizeof(TBeRoXMSampleValue));
  end else begin
   GetMem(Data,(Samples+(BeRoXMSampleFixUpLength*4))*sizeof(TBeRoXMSampleValue));
   FillChar(Data^,(Samples+(BeRoXMSampleFixUpLength*4))*sizeof(TBeRoXMSampleValue),#0);
  end;
  inc(ptruint(Data),(BeRoXMSampleFixUpLength*2)*sizeof(TBeRoXMSampleValue));
 end;
end;

procedure TBeRoXMSample.FixUp;
var Counter,LoopEnd:integer;
begin
 if assigned(Data) then begin
  if SampleLength>0 then begin
   case Channels of
    1:begin
     for Counter:=0 to BeRoXMSampleFixUpLength-1 do begin
      Data^[-(Counter+1)]:=Data^[0];
      Data^[SampleLength+Counter]:=Data^[SampleLength-1];
     end;
     if Loop then begin
      LoopEnd:=LoopStart+LoopLength;
      if (LoopStart>0) and (LoopEnd>0) and (LoopEnd<=SampleLength) then begin
       if PingPongLoop then begin
        for Counter:=0 to BeRoXMSampleFixUpLength-1 do begin
         Data^[LoopEnd+Counter]:=Data^[LoopEnd-(Counter+1)];
        end;
       end else begin
        for Counter:=0 to BeRoXMSampleFixUpLength-1 do begin
         Data^[LoopEnd+Counter]:=Data^[LoopStart+Counter];
        end;
       end;
      end;
     end;
    end;
    2:begin
     for Counter:=0 to BeRoXMSampleFixUpLength-1 do begin
      Data^[(-(Counter+1)*2)]:=Data^[0];
      Data^[(-(Counter+1)*2)+1]:=Data^[1];
      Data^[(SampleLength+Counter)*2]:=Data^[(SampleLength-1)*2];
      Data^[((SampleLength+Counter)*2)+1]:=Data^[((SampleLength-1)*2)+1];
     end;
     if Loop and not PingPongLoop then begin
      LoopEnd:=LoopStart+LoopLength;
      if (LoopStart>0) and (LoopEnd>0) and (LoopEnd<=SampleLength) then begin
       if PingPongLoop then begin
        for Counter:=0 to BeRoXMSampleFixUpLength-1 do begin
         Data^[(LoopEnd+Counter)*2]:=Data^[(LoopEnd-(Counter+1))*2];
         Data^[((LoopEnd+Counter)*2)+1]:=Data^[((LoopEnd-(Counter+1))*2)+1];
        end;
       end else begin
        for Counter:=0 to BeRoXMSampleFixUpLength-1 do begin
         Data^[(LoopEnd+Counter)*2]:=Data^[(LoopStart+Counter)*2];
         Data^[((LoopEnd+Counter)*2)+1]:=Data^[((LoopStart+Counter)*2)+1];
        end;
       end;
      end;
     end;
    end;
   end;
  end;
 end;
end;

constructor TBeRoXMEnvelope.Create;
begin
 inherited Create;
 Clear;
end;

destructor TBeRoXMEnvelope.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TBeRoXMEnvelope.Clear;
begin
 FillChar(Points,sizeof(Points),#0);
 Active:=false;
 Loop:=false;
 Sustain:=false;
 NumberOfPoints:=0;
 LoopStartPoint:=0;
 LoopEndPoint:=0;
 SustainPoint:=0;
end;

constructor TBeRoXMInstrument.Create;
var Counter:integer;
begin
 inherited Create;
 VolumeEnvelope:=TBeRoXMEnvelope.Create;
 PanningEnvelope:=TBeRoXMEnvelope.Create;
 for Counter:=0 to BeRoXMLastSample do Sample[Counter]:=nil;
 Clear;
end;

destructor TBeRoXMInstrument.Destroy;
begin
 Clear;
 VolumeEnvelope.Destroy;
 PanningEnvelope.Destroy;
 inherited Destroy;
end;

procedure TBeRoXMInstrument.Clear;
var Counter:integer;
begin
 for Counter:=0 to BeRoXMLastSample do begin
  if assigned(Sample[Counter]) then Sample[Counter].Destroy;
  Sample[Counter]:=nil;
 end;
 for Counter:=1 to 96 do SampleMap[Counter]:=0;
 VolumeEnvelope.Clear;
 PanningEnvelope.Clear;
 FadeOut:=0;
 VibratoType:=0;
 VibratoSweep:=0;
 VibratoDepth:=0;
 VibratoRate:=0;
end;

constructor TBeRoXMEnvelopeHandler.Create;
begin
 inherited Create;
 Envelope:=nil;
end;

destructor TBeRoXMEnvelopeHandler.Destroy;
begin
 inherited Destroy;
end;

procedure TBeRoXMEnvelopeHandler.Assign(From:TBeRoXMEnvelopeHandler);
begin
 Envelope:=From.Envelope;
 KeyOn:=From.KeyOn;
 CurrentTick:=From.CurrentTick;
 CurrentPoint:=From.CurrentPoint;
 NextPoint:=From.NextPoint;
 CurrentValue:=From.CurrentValue;
 Delta:=From.Delta;
end;

procedure TBeRoXMEnvelopeHandler.Reset(TheTick:integer);
var X1,Y1,X2,Y2,XD,YD,Counter:integer;
begin
 CurrentTick:=TheTick;
 CurrentPoint:=0;
 CurrentValue:=0;
 if assigned(Envelope) then begin
  CurrentPoint:=0;
  for Counter:=0 to Envelope.NumberOfPoints-1 do begin
   if Envelope.Points[Counter].Tick<=CurrentTick then begin
    CurrentPoint:=Counter;
   end else begin
    break;
   end;
  end;
  if Envelope.Loop and (CurrentPoint=Envelope.LoopEndPoint) then begin
   CurrentPoint:=Envelope.LoopStartPoint;
   CurrentTick:=Envelope.Points[CurrentPoint].Tick;
  end;
  if CurrentPoint>=(Envelope.NumberOfPoints-1) then begin
   CurrentValue:=Envelope.Points[Envelope.NumberOfPoints-1].Value*65536;
  end else begin
   X1:=Envelope.Points[CurrentPoint].Tick;
   X2:=Envelope.Points[CurrentPoint+1].Tick;
   Y1:=Envelope.Points[CurrentPoint].Value*65536;
   Y2:=Envelope.Points[CurrentPoint+1].Value*65536;
   XD:=X2-X1;
   YD:=Y2-Y1;
   if XD<>0 then begin
    Delta:=YD div XD;
    CurrentValue:=Y1+(Delta*(CurrentTick-X1));
   end else begin
    Delta:=0;
    CurrentValue:=Y1;
   end;
  end;
 end;
 NextPoint:=CurrentPoint+1;
end;

function TBeRoXMEnvelopeHandler.ProcessTick:integer;
var X1,Y1,X2,Y2,XD,YD:integer;
begin
 if (not assigned(Envelope)) or ((Envelope.NumberOfPoints=0) or not Envelope.Active) then begin
  result:=0;
  exit;
 end;
 if NextPoint<Envelope.NumberOfPoints then begin
  if CurrentTick=Envelope.Points[NextPoint].Tick then begin
   CurrentPoint:=NextPoint;
   if Envelope.Loop and (CurrentPoint=Envelope.LoopEndPoint) then begin
    CurrentPoint:=Envelope.LoopStartPoint;
    CurrentTick:=Envelope.Points[CurrentPoint].Tick;
   end;
   if (CurrentPoint+1)<Envelope.NumberOfPoints then begin
    X1:=Envelope.Points[CurrentPoint].Tick;
    X2:=Envelope.Points[CurrentPoint+1].Tick;
    Y1:=Envelope.Points[CurrentPoint].Value*65536;
    Y2:=Envelope.Points[CurrentPoint+1].Value*65536;
    XD:=X2-X1;
    YD:=Y2-Y1;
    if XD<>0 then begin
     Delta:=YD div XD;
    end else begin
     Delta:=0;
    end;
    CurrentValue:=Y1;
   end else begin
    Delta:=0;
    CurrentValue:=Envelope.Points[Envelope.NumberOfPoints-1].Value*65536;
   end;
   NextPoint:=CurrentPoint+1;
  end;
 end;          
{$ifdef HasSAR}
 result:=SARLongint(CurrentValue,16);
{$else}
{$ifdef UseSAR}
 result:=sar(CurrentValue,16);
{$else}
 result:=CurrentValue div 65536;
{$endif}
{$endif}
 if (NextPoint<Envelope.NumberOfPoints) and not ((CurrentTick=Envelope.Points[CurrentPoint].Tick) and (Envelope.Sustain and ((CurrentPoint=Envelope.SustainPoint) and KeyOn))) then begin
  inc(CurrentValue,Delta);
  inc(CurrentTick);
 end;
end;

constructor TBeRoXMChannel.Create(TheModule:TBeRoXMModule;TheNumber:integer;VirtualChannel,ClickRemovalFadeOutChannel:boolean);
begin
 inherited Create;
 IsVirtualChannel:=VirtualChannel;
 IsClickRemovalFadeOutChannel:=ClickRemovalFadeOutChannel;
 Number:=TheNumber;
 Plugin:=0;
 AudioProcessChain:=0;
 Module:=TheModule;
 VolumeEnvelope:=TBeRoXMEnvelopeHandler.Create;
 PanningEnvelope:=TBeRoXMEnvelopeHandler.Create;
 Name:='';
 StandardPanning:=128;
 Clear;
end;

destructor TBeRoXMChannel.Destroy;
begin
 VolumeEnvelope.Destroy;
 PanningEnvelope.Destroy;
 inherited Destroy;
end;

procedure TBeRoXMChannel.Assign(From:TBeRoXMChannel);
begin
 Name:=From.Name;
 Active:=From.Active;
 Module:=From.Module;
 Number:=From.Number;
 Plugin:=From.Plugin;
 AudioProcessChain:=From.AudioProcessChain;
 Sample:=From.Sample;
 Instrument:=From.Instrument;
 Increment:=From.Increment;
 SampleIncrement:=From.SampleIncrement;
 SamplePosition:=From.SamplePosition;
{$ifdef MixerIntegerIncrement}
 SampleIntegerIncrement:=From.SampleIntegerIncrement;
 SampleIntegerPosition:=From.SampleIntegerPosition;
 SampleIntegerSubPosition:=From.SampleIntegerSubPosition;
{$endif}
 SampleBackwards:=From.SampleBackwards;
 MixerVolume:=From.MixerVolume;
 MixerPanning:=From.MixerPanning;
 MixerFrequency:=From.MixerFrequency;
 EnvelopeVolume:=From.EnvelopeVolume;
 EnvelopePanning:=From.EnvelopePanning;
 VolumeEnvelope.Assign(From.VolumeEnvelope);
 PanningEnvelope.Assign(From.PanningEnvelope);
 CurrentNote:=From.CurrentNote;
 Tick:=From.Tick;
 VibratoType:=From.VibratoType;
 VibratoSpeed:=From.VibratoSpeed;
 VibratoDepth:=From.VibratoDepth;
 VibratoPosition:=From.VibratoPosition;
 TremoloSpeed:=From.TremoloSpeed;
 TremoloDepth:=From.TremoloDepth;
 TremoloPosition:=From.TremoloPosition;
 TremorPosition:=From.TremorPosition;
 TremorOn:=From.TremorOn;
 TremorOff:=From.TremorOff;
 InstrumentVibratoSweepPosition:=From.InstrumentVibratoSweepPosition;
 InstrumentVibratoPosition:=From.InstrumentVibratoPosition;
 MultiRetrigAction:=From.MultiRetrigAction;
 MultiRetrigOnTick:=From.MultiRetrigOnTick;
 Period:=From.Period;
 ArpeggioPeriodDelta:=From.ArpeggioPeriodDelta;
 PeriodDelta:=From.PeriodDelta;
 DestPeriod:=From.DestPeriod;
 Volume:=From.Volume;
 StandardPanning:=From.StandardPanning;
 Panning:=From.Panning;
 FadeOut:=From.FadeOut;
 VolumeSlideParameter:=From.VolumeSlideParameter;
 GlobalVolumeSlideParameter:=From.GlobalVolumeSlideParameter;
 TonePortaParameter:=From.TonePortaParameter;
 PortaUpParameter:=From.PortaUpParameter;
 PortaDownParameter:=From.PortaDownParameter;
 FinePortaUpParameter:=From.FinePortaUpParameter;
 FinePortaDownParameter:=From.FinePortaDownParameter;
 ExtraFinePortaUpParameter:=From.ExtraFinePortaUpParameter;
 ExtraFinePortaDownParameter:=From.ExtraFinePortaDownParameter;
 FineVolumeUpParameter:=From.FineVolumeUpParameter;
 FineVolumeDownParameter:=From.FineVolumeDownParameter;
 PanningSlideParameter:=From.PanningSlideParameter;
 VolumeDelta:=From.VolumeDelta;
 Glissando:=From.Glissando;
 TremoloType:=From.TremoloType;
 FineTune:=From.FineTune;
 KeyOff:=From.KeyOff;
 PatternLoopStart:=From.PatternLoopStart;
 PatternLoopCounter:=From.PatternLoopCounter;
 PatternLoop:=From.PatternLoop;
 LastLeft:=From.LastLeft;
 LastRight:=From.LastRight;
 VolumeLeft:=From.VolumeLeft;
 VolumeRight:=From.VolumeRight;
 VolumeRampingLeft:=From.VolumeRampingLeft;
 VolumeRampingRight:=From.VolumeRampingRight;
 DestVolumeRampingLeft:=From.DestVolumeRampingLeft;
 DestVolumeRampingRight:=From.DestVolumeRampingRight;
 StepVolumeRampingLeft:=From.StepVolumeRampingLeft;
 StepVolumeRampingRight:=From.StepVolumeRampingRight;
 VolumeRampingCounter:=From.VolumeRampingCounter;
 RealNote:=From.RealNote;
 CutOff:=From.CutOff;
 Resonance:=From.Resonance;
 ActiveMacro:=From.ActiveMacro;
 FilterActive:=From.FilterActive;
 FilterMode:=From.FilterMode;
 FilterA0:=From.FilterA0;
 FilterB0:=From.FilterB0;
 FilterB1:=From.FilterB1;
 FilterY1:=From.FilterY1;
 FilterY2:=From.FilterY2;
 FilterY3:=From.FilterY3;
 FilterY4:=From.FilterY4;
 SmoothInitialValue:=From.SmoothInitialValue;
 SmoothValueStep:=From.SmoothValueStep;
 Surround:=From.Surround;
 LocalFilterMode:=From.LocalFilterMode;
end;

procedure TBeRoXMChannel.Clear;
begin
 Reset;
 Instrument:=nil;
 Sample:=nil;
 Panning:=StandardPanning;
 LastLeft:=0;
 LastRight:=0;
 VolumeLeft:=0;
 VolumeRight:=0;
 VolumeRampingLeft:=0;
 VolumeRampingRight:=0;
 DestVolumeRampingLeft:=0;
 DestVolumeRampingRight:=0;
 StepVolumeRampingLeft:=0;
 StepVolumeRampingRight:=0;
 RealNote:=0;
 FillChar(CurrentNote,sizeof(TBeRoXMPatternNote),#0);
 CutOff:=$7f;
 Resonance:=0;
 ActiveMacro:=0;
 FilterActive:=false;
 FilterMode:=BeRoXMCutOffLowPass;
 Surround:=false;
 LocalFilterMode:=false;
end;

procedure TBeRoXMChannel.Reset;
begin
 Active:=false;
 VolumeEnvelope.Envelope:=nil;
 PanningEnvelope.Envelope:=nil;
 VolumeEnvelope.Reset(0);
 PanningEnvelope.Reset(0);
 VolumeEnvelope.KeyOn:=true;
 PanningEnvelope.KeyOn:=true;
 KeyOff:=false;
 InstrumentVibratoSweepPosition:=0;
 InstrumentVibratoPosition:=0;
 if VibratoType<4 then VibratoPosition:=0;
 if TremoloType<4 then TremoloPosition:=0;
 TremorPosition:=0;
 FadeOut:=65536;
end;

function TBeRoXMChannel.StartClickRemovalFadeOut:boolean;
var Counter,Found,BestFound,BestValue:integer;
    Target:TBeRoXMChannel;
    TargetAudioProcessChain:TBeRoXMAudioProcessChain;
begin
 result:=false;
 if Active and assigned(Module) then begin
  Found:=-1;
  for Counter:=0 to BeRoXMLastChannel do begin
   if not Module.ClickRemovalFadeOutChannel[Counter].Active then begin
    Found:=Counter;
    break;
   end;
  end;
  if Found<0 then begin
   BestFound:=-1;
   BestValue:=$7fffffff;
   for Counter:=0 to BeRoXMLastChannel do begin
    if Module.ClickRemovalFadeOutChannel[Counter].MixerVolume<BestValue then begin
     BestFound:=Counter;
     BestValue:=Module.ClickRemovalFadeOutChannel[Counter].Volume;
    end;
   end;
   Found:=BestFound;
  end;
  if Found<0 then begin
   BestFound:=-1;
   BestValue:=$7fffffff;
   for Counter:=0 to BeRoXMLastChannel do begin
    if Module.ClickRemovalFadeOutChannel[Counter].Volume<BestValue then begin
     BestFound:=Counter;
     BestValue:=Module.ClickRemovalFadeOutChannel[Counter].Volume;
    end;
   end;
   Found:=BestFound;
  end;
  if Found>=0 then begin
   Target:=Module.ClickRemovalFadeOutChannel[Found];
   if Target.Active then begin
    if (Target.AudioProcessChain>0) and (assigned(Module.AudioProcessChains[Target.AudioProcessChain]) and Module.AudioProcessChains[Target.AudioProcessChain].Active) then begin
     TargetAudioProcessChain:=Module.AudioProcessChains[Target.AudioProcessChain];
     TargetAudioProcessChain.LastLeft:=TargetAudioProcessChain.LastLeft+Target.LastLeft;
     TargetAudioProcessChain.LastRight:=TargetAudioProcessChain.LastRight+Target.LastRight;
    end else begin
     Module.LastLeft:=Module.LastLeft+Target.LastLeft;
     Module.LastRight:=Module.LastRight+Target.LastRight;
    end;
    Target.LastLeft:=0;
    Target.LastRight:=0;
   end;
   Target.Assign(self);
   Target.FadeOut:=0;
   Active:=false;
   result:=true;
  end else begin
   if (AudioProcessChain>0) and (assigned(Module.AudioProcessChains[AudioProcessChain]) and Module.AudioProcessChains[AudioProcessChain].Active) then begin
    TargetAudioProcessChain:=Module.AudioProcessChains[AudioProcessChain];
    TargetAudioProcessChain.LastLeft:=TargetAudioProcessChain.LastLeft+LastLeft;
    TargetAudioProcessChain.LastRight:=TargetAudioProcessChain.LastRight+LastRight;
   end else begin
    Module.LastLeft:=Module.LastLeft+LastLeft;
    Module.LastRight:=Module.LastRight+LastRight;
   end;
   LastLeft:=0;
   LastRight:=0;
   Active:=false;
  end;
 end;
end;

procedure TBeRoXMChannel.UpdateArpeggio;
begin
 inc(PeriodDelta,ArpeggioPeriodDelta[Module.Tick mod 3]);
end;

procedure TBeRoXMChannel.UpdateAutoVibrato;
var Value:integer;
begin
 if assigned(Instrument) then begin
  case Instrument.VibratoType of
{$ifdef UseTables}
   0:Value:=AutoVibratoSinusTable[InstrumentVibratoPosition and $ff];
{$else}
   0:Value:=trunc(sin(2*pi*InstrumentVibratoPosition/256)*64);
{$endif}
   1:Value:=64*(1-(2*ord(InstrumentVibratoPosition>=128)));
{$ifdef HasSAR}
   2:Value:=SARLongint(128-((InstrumentVibratoPosition+128) and $ff),1);
   3:Value:=SARLongint(128-(((256-InstrumentVibratoPosition)+128) and $ff),1);
{$else}
{$ifdef UseSAR}
   2:Value:=sar(128-((InstrumentVibratoPosition+128) and $ff),1);
   3:Value:=sar(128-(((256-InstrumentVibratoPosition)+128) and $ff),1);
{$else}
   2:Value:=(128-((InstrumentVibratoPosition+128) and $ff)) div 2;
   3:Value:=(128-(((256-InstrumentVibratoPosition)+128) and $ff)) div 2;
{$endif}
{$endif}
   else Value:=0;
  end;
  Value:=Value*Instrument.VibratoDepth;
  if Instrument.VibratoSweep<>0 then begin
   Value:=(Value*InstrumentVibratoSweepPosition) div Instrument.VibratoSweep;
   if not KeyOff then begin
    inc(InstrumentVibratoSweepPosition);
   end;
   if InstrumentVibratoSweepPosition>Instrument.VibratoSweep then begin
    InstrumentVibratoSweepPosition:=Instrument.VibratoSweep;
   end;
  end;
{$ifdef HasSAR}
  Value:=SARLongint(Value,6);
{$else}
{$ifdef UseSAR}
  Value:=sar(Value,6);
{$else}
  Value:=Value div 64;
{$endif}
{$endif}
  inc(PeriodDelta,Value);
  InstrumentVibratoPosition:=(InstrumentVibratoPosition+Instrument.VibratoRate) and $ff;
 end;
end;

procedure TBeRoXMChannel.UpdateVibrato;
var Value:integer;
begin
 case VibratoType of
{$ifdef UseTables}
  0..3:Value:=ModulationTable[VibratoType,VibratoPosition and $3f]*2;
{$else}
  0:Value:=trunc(sin(2*pi*VibratoPosition/64)*256);
  1:Value:=255-(((VibratoPosition+32) and $3f)*8);
  2:Value:=255*(1-(2*ord(VibratoPosition>=32)));
  3:Value:=random(512)-256;
{$endif}
  else Value:=0;
 end;
{$ifdef HasSAR}
 Value:=SARLongint(Value*VibratoDepth,7)*4;
{$else}
{$ifdef UseSAR}
 Value:=sar(Value*VibratoDepth,7)*4;
{$else}
 Value:=((Value*VibratoDepth) div 128)*4;
{$endif}
{$endif}
 inc(PeriodDelta,Value);
 VibratoPosition:=(VibratoPosition+VibratoSpeed) and $3f;
end;

procedure TBeRoXMChannel.UpdateTremolo;
var Value:integer;
begin
 case TremoloType of
{$ifdef UseTables}
  0..3:Value:=ModulationTable[TremoloType,TremoloPosition and $3f]*2;   
{$else}
  0:Value:=trunc(sin(2*pi*TremoloPosition/64)*256);
  1:Value:=255-(((TremoloPosition+32) and $3f)*8);
  2:Value:=255*(1-(2*ord(TremoloPosition>=32)));
  3:Value:=random(512)-256;
{$endif}
  else Value:=0;
 end;
{$ifdef HasSAR}
 inc(VolumeDelta,SARLongint(Value*TremoloDepth,6));
{$else}
{$ifdef UseSAR}
 inc(VolumeDelta,sar(Value*TremoloDepth,6));
{$else}
 inc(VolumeDelta,(Value*TremoloDepth) div 64);
{$endif}
{$endif}
 TremoloPosition:=(TremoloPosition+TremoloSpeed) and $3f;
end;

procedure TBeRoXMChannel.UpdateTremor;
begin
 if (TremorOn+TremorOff)<>0 then begin
  if TremorPosition>=TremorOn then begin
   dec(VolumeDelta,Volume);
  end;
  TremorPosition:=(TremorPosition+1) mod (TremorOn+TremorOff);
 end;
end;

procedure TBeRoXMChannel.UpdateTonePorta;
var NextPeriod:integer;
begin
 NextPeriod:=0;
 if Period<DestPeriod then begin
  if Glissando then begin
   NextPeriod:=Module.GetPeriod(Module.GetNote(Period,FineTune)+1,FineTune);
  end;
  inc(Period,TonePortaParameter*4);
  if Period>DestPeriod then begin
   Period:=DestPeriod;
  end;
  if Glissando then begin
   if Period>DestPeriod then begin
    Period:=DestPeriod;
   end;
   if Period>NextPeriod then begin
    Period:=NextPeriod;
   end;
  end;
 end else if Period>DestPeriod then begin
  if Glissando then begin
   NextPeriod:=Module.GetPeriod(Module.GetNote(Period,FineTune)-1,FineTune);
  end;
  dec(Period,TonePortaParameter*4);
  if Period<DestPeriod then begin
   Period:=DestPeriod;
  end;
  if Glissando then begin
   if Period<DestPeriod then begin
    Period:=DestPeriod;
   end;
   if Period<NextPeriod then begin
    Period:=NextPeriod;
   end;
  end;
 end;
end;

procedure TBeRoXMChannel.UpdateVolumeSlide;
begin
 inc(Volume,VolumeSlideParameter);
end;

{$ifdef UseMIDIMacros}
procedure TBeRoXMChannel.UpdateMacro(Effect,EffectParameter:integer);
begin
 if EffectParameter<$80 then begin
  ProcessMacro(Module.MIDIConfig.SFXExt[ActiveMacro],EffectParameter,Effect=BeRoXMEffectSmoothMacro);
 end else begin
  ProcessMacro(Module.MIDIConfig.ZXXExt[EffectParameter and $7f],0,Effect=BeRoXMEffectSmoothMacro);
 end;
end;
{$endif}

procedure TBeRoXMChannel.UpdateEnvelopes;
begin
 EnvelopeVolume:=64;
 EnvelopePanning:=32;
 if assigned(VolumeEnvelope.Envelope) then begin
  if VolumeEnvelope.Envelope.Active then begin
   EnvelopeVolume:=VolumeEnvelope.ProcessTick;
  end else if not VolumeEnvelope.KeyOn then begin
   EnvelopeVolume:=0;
  end;
  if assigned(Instrument) and not VolumeEnvelope.KeyOn then begin
   dec(FadeOut,Instrument.FadeOut*2);
   if FadeOut<0 then begin
    FadeOut:=0;
   end;
  end;
 end;
 if assigned(PanningEnvelope.Envelope) then begin
  if PanningEnvelope.Envelope.Active then begin
   EnvelopePanning:=PanningEnvelope.ProcessTick;
  end;
 end;
end;

procedure TBeRoXMChannel.UpdateTick;
var EndVolume:integer;
begin
 if assigned(Instrument) then begin
  UpdateEnvelopes;
  UpdateAutoVibrato;
  if Active then begin
   if Volume<0 then begin
    Volume:=0;
   end else if Volume>64 then begin
    Volume:=64;
   end;
   if Module.GlobalVolume<0 then begin
    Module.GlobalVolume:=0;
   end else if Module.GlobalVolume>64 then begin
    Module.GlobalVolume:=64;
   end;
   if Module.MasterVolume<0 then begin
    Module.MasterVolume:=0;
   end else if Module.MasterVolume>256 then begin
    Module.MasterVolume:=256;
   end;
   if VolumeDelta<0 then begin
    VolumeDelta:=0;
   end else if VolumeDelta>64 then begin
    VolumeDelta:=64;
   end;
   EndVolume:=Volume+VolumeDelta;
   if EndVolume<0 then begin
    EndVolume:=0;
   end else if EndVolume>64 then begin
    EndVolume:=64;
   end;
   if Panning<0 then begin
    Panning:=0;
   end else if Panning>255 then begin
    Panning:=255;
   end;
   if EnvelopeVolume<0 then begin
    EnvelopeVolume:=0;
   end else if EnvelopeVolume>64 then begin
    EnvelopeVolume:=64;
   end;
   if Period<BeRoXMMinPeriod then begin
    Period:=BeRoXMMinPeriod;
   end else if Period>BeRoXMMaxPeriod then begin
    Period:=BeRoXMMaxPeriod;
   end;
{$ifdef HasSAR}
   MixerVolume:=SARLongint(BeRoXMVolumeLength*FadeOut,16);
   MixerVolume:=SARLongint(MixerVolume*EnvelopeVolume,6);
   MixerVolume:=SARLongint(MixerVolume*Module.GlobalVolume,6);
   MixerVolume:=SARLongint(MixerVolume*EndVolume,6);
   MixerVolume:=SARLongint(MixerVolume*Module.MasterVolume,8);
   MixerPanning:=Panning+SARLongint((EnvelopePanning-32)*(128-abs(Panning-128)),5);
{$else}
{$ifdef UseSAR}
   MixerVolume:=sar(BeRoXMVolumeLength*FadeOut,16);
   MixerVolume:=sar(MixerVolume*EnvelopeVolume,6);
   MixerVolume:=sar(MixerVolume*Module.GlobalVolume,6);
   MixerVolume:=sar(MixerVolume*EndVolume,6);
   MixerVolume:=sar(MixerVolume*Module.MasterVolume,8);
   MixerPanning:=Panning+sar((EnvelopePanning-32)*(128-abs(Panning-128)),5);
{$else}
   MixerVolume:=(BeRoXMVolumeLength*FadeOut) div 65536;
   MixerVolume:=(MixerVolume*EnvelopeVolume) div 64;
   MixerVolume:=(MixerVolume*Module.GlobalVolume) div 64;
   MixerVolume:=(MixerVolume*EndVolume) div 64;
   MixerVolume:=(MixerVolume*Module.MasterVolume) div 256;
   MixerPanning:=Panning+(((EnvelopePanning-32)*(128-abs(Panning-128))) div 32);
{$endif}
{$endif}
   if Module.Owner.OutputChannels=1 then begin
    VolumeLeft:=MixerVolume;
    DestVolumeRampingLeft:=VolumeLeft*BeRoXMRampLength;
    StepVolumeRampingLeft:=DestVolumeRampingLeft-VolumeRampingLeft;
    if IsClickRemovalFadeOutChannel then begin
     VolumeRampingCounter:=Module.FastRampSamples;
    end else begin
     VolumeRampingCounter:=Module.RampSamples;
    end;
    if VolumeRampingCounter>Module.TickSamples then begin
     VolumeRampingCounter:=Module.TickSamples;
    end;
    if VolumeRampingCounter<>0 then begin
     StepVolumeRampingLeft:=StepVolumeRampingLeft div VolumeRampingCounter;
    end;
   end else begin
    if Surround then begin
     VolumeLeft:=MixerVolume;
     VolumeRight:=-MixerVolume;
    end else begin
{$ifdef HasSAR}
     VolumeLeft:=SARLongint((255-MixerPanning)*MixerVolume,8);
     VolumeRight:=SARLongint(MixerPanning*MixerVolume,8);
{$else}
{$ifdef UseSAR}
     VolumeLeft:=sar((255-MixerPanning)*MixerVolume,8);
     VolumeRight:=sar(MixerPanning*MixerVolume,8);
{$else}
     VolumeLeft:=((255-MixerPanning)*MixerVolume) div 256;
     VolumeRight:=(MixerPanning*MixerVolume) div 256;
{$endif}
{$endif}
    end;
    DestVolumeRampingLeft:=VolumeLeft*BeRoXMRampLength;
    DestVolumeRampingRight:=VolumeRight*BeRoXMRampLength;
    StepVolumeRampingLeft:=DestVolumeRampingLeft-VolumeRampingLeft;
    StepVolumeRampingRight:=DestVolumeRampingRight-VolumeRampingRight;
    if IsClickRemovalFadeOutChannel then begin
     VolumeRampingCounter:=Module.FastRampSamples;
    end else begin
     VolumeRampingCounter:=Module.RampSamples;
    end;
    if VolumeRampingCounter>Module.TickSamples then begin
     VolumeRampingCounter:=Module.TickSamples;
    end;
    if VolumeRampingCounter<>0 then begin                    
     StepVolumeRampingLeft:=StepVolumeRampingLeft div VolumeRampingCounter;
     StepVolumeRampingRight:=StepVolumeRampingRight div VolumeRampingCounter;
    end;
   end;
   MixerFrequency:=Module.GetFrequency(Period+PeriodDelta);
   if Module.SampleRate<>0 then begin
{$ifdef MixerFloatingPointIncrementCalculation}
    Increment:=round((MixerFrequency/Module.SampleRate)*BeRoXMPositionFactorIncrement);
{$else}
    Increment:=(MixerFrequency*BeRoXMPositionFactorIncrement) div Module.SampleRate;
{$endif}
   end else begin
    Increment:=0;
   end;
  end;
 end else begin
  Active:=false;
 end;
end;

procedure TBeRoXMChannel.ProcessRow(const Note:TBeRoXMPatternNote);
var EffectParameterHi,EffectParameterLo,StartTick:integer;
begin
 EffectParameterHi:=Note.EffectParameter shr 4;
 EffectParameterLo:=Note.EffectParameter and $f;

 if (Note.Effect=BeRoXMEffectExtendedEffects) and
    (EffectParameterHi=BeRoXMEffectExtendedEffectNoteDelay) then begin
  StartTick:=EffectParameterLo;
 end else begin
  StartTick:=0;
 end;

 PeriodDelta:=0;
 VolumeDelta:=0;
 
 if StartTick=Tick then begin
  ProcessTrigger(Note);
 end;

 case Note.Volume of
  // Volume
  $10..$50:begin
   if StartTick=Tick then begin
    Volume:=Note.Volume-$10;
   end;
  end;

  // Fine volume slide down
  $80..$8f:begin
   if StartTick=Tick then begin
    dec(Volume,Note.Volume and $f);
   end;
  end;

  // Fine volume slide up
  $90..$9f:begin
   if StartTick=Tick then begin
    inc(Volume,Note.Volume and $f);
   end;
  end;

  // Set vibrato speed
  $a0..$af:begin
   VibratoSpeed:=Note.Volume and $f;
  end;

  // Set vibrato depth
  $b0..$bf:begin
   VibratoDepth:=Note.Volume and $f;
  end;

  // Set panning
  $c0..$cf:begin
   if StartTick=Tick then begin
    Panning:=(Note.Volume and $f) shl 4;
   end;
  end;

  // Tone porta
  $f0..$ff:begin
   TonePortaParameter:=Note.Volume and $f;
  end;
 end;

 if StartTick=Tick then begin
  case Note.Effect of
   BeRoXMEffectArpeggio:begin
    if Note.EffectParameter<>0 then begin
     ArpeggioPeriodDelta[0]:=0;
     if Module.LinearSlides then begin
      ArpeggioPeriodDelta[1]:=-EffectParameterHi*(16*4);
      ArpeggioPeriodDelta[2]:=-EffectParameterLo*(16*4);
     end else begin            
      ArpeggioPeriodDelta[1]:=Module.GetPeriod(RealNote+EffectParameterHi,FineTune)-Module.GetPeriod(RealNote,FineTune);
      ArpeggioPeriodDelta[2]:=Module.GetPeriod(RealNote+EffectParameterLo,FineTune)-Module.GetPeriod(RealNote,FineTune);
     end;
    end;
   end;
   BeRoXMEffectPortaUp:begin
    if Note.EffectParameter<>0 then begin
     PortaUpParameter:=Note.EffectParameter;
    end;
   end;
   BeRoXMEffectPortaDown:begin
    if Note.EffectParameter<>0 then begin
     PortaDownParameter:=Note.EffectParameter;
    end;
   end;
   BeRoXMEffectTonePorta:begin
    if Note.EffectParameter<>0 then begin
     TonePortaParameter:=Note.EffectParameter;
    end;
   end;
   BeRoXMEffectVibrato:begin
    if Note.EffectParameter<>0 then begin
     VibratoSpeed:=EffectParameterHi;
     VibratoDepth:=EffectParameterlo;
    end;
    UpdateVibrato;
   end;
   BeRoXMEffectVibratoVolumeSlide:begin
    if Note.EffectParameter<>0 then begin
     VolumeSlideParameter:=EffectParameterHi-EffectParameterLo;
    end;
    UpdateVibrato;
   end;
   BeRoXMEffectTonePortaVolumeSlide,
   BeRoXMEffectVolumeSlide:begin
    if Note.EffectParameter<>0 then begin
     VolumeSlideParameter:=EffectParameterHi-EffectParameterLo;
    end;
   end;
   BeRoXMEffectTremolo:begin
    if Note.EffectParameter<>0 then begin
     TremoloSpeed:=EffectParameterHi;
     TremoloDepth:=EffectParameterLo;
    end;
   end;
   BeRoXMEffectPanning:begin
    Panning:=Note.EffectParameter;
   end;
   BeRoXMEffectSampleOffset:begin
    SamplePosition:=(Note.EffectParameter*256)*BeRoXMPositionFactor;
   end;
   BeRoXMEffectPatternJump:begin
    Module.NextPatternOrder:=Note.EffectParameter;
    if not Module.Jump then begin
     Module.NextPatternRow:=0;
    end;
    Module.Jump:=true;
   end;
   BeRoXMEffectVolume:begin
    Volume:=Note.EffectParameter;
   end;
   BeRoXMEffectPatternBreak:begin
    if not Module.Jump then begin
     Module.NextPatternOrder:=Module.CurrentPatternOrder+1;
    end;
    Module.NextPatternRow:=Note.EffectParameter;
    Module.Jump:=true;
   end;
   BeRoXMEffectExtendedEffects:begin
    case EffectParameterHi of
     BeRoXMEffectExtendedEffectFinePortaUp:begin
      if EffectParameterLo<>0 then begin
       FinePortaUpParameter:=EffectParameterLo;
      end;
      dec(Period,FinePortaUpParameter*4);
      DestPeriod:=Period;
     end;
     BeRoXMEffectExtendedEffectFinePortaDown:begin
      if EffectParameterLo<>0 then begin
       FinePortaDownParameter:=EffectParameterLo;
      end;
      inc(Period,FinePortaDownParameter*4);
      DestPeriod:=Period;
     end;
     BeRoXMEffectExtendedEffectSetGlissControl:begin
      Glissando:=EffectParameterLo<>0;
     end;
     BeRoXMEffectExtendedEffectSetVibratotype:begin
      VibratoType:=EffectParameterLo and 7;
     end;
     BeRoXMEffectExtendedEffectSetFineTune:begin
      Period:=Module.GetPeriod(Module.GetNote(Period,FineTune),EffectParameterLo);
      FineTune:=EffectParameterLo;
     end;
     BeRoXMEffectExtendedEffectPatternLoop:begin
      if EffectParameterLo=0 then begin
       if not PatternLoop then begin
        PatternLoopStart:=Module.CurrentPatternRow;
       end;
      end else begin
       if PatternLoop then begin
        if PatternLoopCounter=0 then begin
         PatternLoop:=false;
        end else begin
         dec(PatternLoopCounter);
         Module.NextPatternOrder:=Module.CurrentPatternOrder;
         Module.NextPatternRow:=PatternLoopStart;
         Module.Jump:=true;
        end;
       end else begin
        PatternLoopCounter:=EffectParameterLo;
        PatternLoop:=true;
        Module.NextPatternOrder:=Module.CurrentPatternOrder;
        Module.NextPatternRow:=PatternLoopStart;
        Module.Jump:=true;
       end;
      end;
     end;
     BeRoXMEffectExtendedEffectSetTremoloType:begin
      TremoloType:=EffectParameterLo and 7;
     end;
     BeRoXMEffectExtendedEffectRetrig:begin
     end;
     BeRoXMEffectExtendedEffectFineVolumeSlideUp:begin
      if EffectParameterLo<>0 then begin
       FineVolumeUpParameter:=EffectParameterLo;
      end;
      inc(Volume,FineVolumeUpParameter);
     end;
     BeRoXMEffectExtendedEffectFineVolumeSlideDown:begin
      if EffectParameterLo<>0 then begin
       FineVolumeDownParameter:=EffectParameterLo;
      end;
      dec(Volume,FineVolumeDownParameter);
     end;
     BeRoXMEffectExtendedEffectNoteCut:begin
     end;
     BeRoXMEffectExtendedEffectNoteDelay:begin
     end;
     BeRoXMEffectExtendedEffectPatternDelay:begin
      Module.PatternDelay:=EffectParameterLo;
     end;
     BeRoXMEffectExtendedEffectSetActiveMacro:begin
      ActiveMacro:=EffectParameterLo;
     end;
    end;
   end;
   BeRoXMEffectSpeedTempo:begin
    if Note.EffectParameter<$20 then begin
     Module.Speed:=Note.EffectParameter;
    end else begin
     Module.Tempo:=Note.EffectParameter;
    end;
   end;
   BeRoXMEffectGlobalVolume:begin
    Module.GlobalVolume:=Note.EffectParameter;
   end;
   BeRoXMEffectGlobalVolumeSlide:begin
    if Note.EffectParameter<>0 then begin
     GlobalVolumeSlideParameter:=EffectParameterHi-EffectParameterLo;
    end;
   end;
   BeRoXMEffectKeyOff:begin
    VolumeEnvelope.KeyOn:=false;
    PanningEnvelope.KeyOn:=false;
    KeyOff:=true;
   end;
   BeRoXMEffectSetEnvelopePosition:begin
    VolumeEnvelope.Reset(Note.EffectParameter);
    PanningEnvelope.Reset(Note.EffectParameter);
   end;
   BeRoXMEffectPanningSlide:begin
    if Note.EffectParameter<>0 then begin
     PanningSlideParameter:=EffectParameterHi-EffectParameterLo;
    end;
   end;
   BeRoXMEffectMultiRetrig:begin
    if Note.EffectParameter<>0 then begin
     MultiRetrigAction:=EffectParameterHi;
     MultiRetrigOnTick:=EffectParameterLo;
    end;
   end;
   BeRoXMEffectResonance:begin
{$ifdef UseFilters}
    Resonance:=Note.EffectParameter;
    SetCutOffFilter(not FilterActive);
{$endif}
   end;
   BeRoXMEffectTremor:begin
    if Note.EffectParameter<>0 then begin
     TremorOn:=EffectParameterHi;
     TremorOff:=EffectParameterLo;
    end;
    UpdateTremor;
   end;
   BeRoXMEffectExtraFinePorta:begin
    case EffectParameterHi of
     $1:begin
      if EffectParameterLo<>0 then begin
       ExtraFinePortaUpParameter:=EffectParameterLo;
      end;
      dec(Period,ExtraFinePortaUpParameter);
      DestPeriod:=Period;
     end;
     $2:begin
      if EffectParameterLo<>0 then begin
       ExtraFinePortaDownParameter:=EffectParameterLo;
      end;
      inc(Period,ExtraFinePortaDownParameter);
      DestPeriod:=Period;
     end;
     $9:begin
      case EffectParameterLo of
       $0:Surround:=true;
       $1:Surround:=false;
       $c:LocalFilterMode:=false;
       $d:LocalFilterMode:=true;
       $e:SampleBackwards:=false;
       $f:SampleBackwards:=true;
      end;
     end;
    end;
   end;
   BeRoXMEffectMacro,BeRoXMEffectSmoothMacro:begin
{$ifdef UseMIDIMacros}
    UpdateMacro(Note.Effect,Note.EffectParameter);
{$endif}
   end;
  end;
 end;
end;

procedure TBeROXMChannel.ProcessTick(const Note:TBeRoXMPatternNote);
var EffectParameterHi,EffectParameterLo,StartTick:integer;
begin
 EffectParameterHi:=Note.EffectParameter shr 4;
 EffectParameterLo:=Note.EffectParameter and $f;

 if (Note.Effect=BeRoXMEffectExtendedEffects) and
    (EffectParameterHi=BeRoXMEffectExtendedEffectNoteDelay) then begin
  StartTick:=EffectParameterLo;
 end else begin
  StartTick:=0;
 end;

 if (StartTick>0) and (StartTick=Tick) and not (IsVirtualChannel or IsClickRemovalFadeOutChannel) then begin
  ProcessRow(Note);
 end;

 PeriodDelta:=0;
 VolumeDelta:=0;

 case Note.Volume of
  // Volume slide down
  $60..$6f:begin
   if Tick>=StartTick then begin
    dec(Volume,Note.Volume and $f);
   end;
  end;

  // Volume slide up
  $70..$7f:begin
   if Tick>=StartTick then begin
    inc(Volume,Note.Volume and $f);
   end;
  end;

  // Set vibrato depth
  $b0..$bf:begin
   UpdateVibrato;
  end;

  // Panning slide left
  $d0..$df:begin
   if Tick>=StartTick then begin
    dec(Panning,Note.Volume and $f);
   end;
  end;

  // Panning slide right
  $e0..$ef:begin
   if Tick>=StartTick then begin
    inc(Panning,Note.Volume and $f);
   end;
  end;

  // Tone porta
  $f0..$ff:begin
   UpdateTonePorta;
  end;
 end;

 if StartTick<>Tick then begin
  case Note.Effect of
   BeRoXMEffectArpeggio:begin
    if Note.EffectParameter<>0 then begin
     UpdateArpeggio;
    end;
   end;
   BeRoXMEffectPortaUp:begin
    dec(Period,PortaUpParameter*4);
    DestPeriod:=Period;
   end;
   BeRoXMEffectPortaDown:begin
    inc(Period,PortaDownParameter*4);
    DestPeriod:=Period;
   end;
   BeRoXMEffectTonePorta:begin
    UpdateTonePorta;
   end;
   BeRoXMEffectVibrato:begin
    UpdateVibrato;
   end;
   BeRoXMEffectTonePortaVolumeSlide:begin
    UpdateTonePorta;
    UpdateVolumeSlide;
   end;
   BeRoXMEffectVibratoVolumeSlide:begin
    UpdateVibrato;
    UpdateVolumeSlide;
   end;
   BeRoXMEffectTremolo:begin
    UpdateTremolo;
   end;
   BeRoXMEffectVolumeSlide:begin
    UpdateVolumeSlide;
   end;
   BeRoXMEffectExtendedEffects:begin
    case EffectParameterHi of
     BeRoXMEffectExtendedEffectRetrig:begin
      if EffectParameterLo<>0 then begin
       if (Module.Tick mod EffectParameterLo)=0 then begin
        ProcessTrigger(Note);
       end;
      end;
     end;
     BeRoXMEffectExtendedEffectNoteCut:begin
      if Module.Tick=EffectParameterLo then begin
       Volume:=0;
      end;
     end;
     BeRoXMEffectExtendedEffectNoteDelay:begin
      if Module.Tick=EffectParameterLo then begin
       ProcessTrigger(Note);
      end;
     end;
    end;
   end;
   BeRoXMEffectGlobalVolumeSlide:begin
    inc(Module.GlobalVolume,GlobalVolumeSlideParameter);
   end;
   BeRoXMEffectPanningSlide:begin
    inc(Panning,PanningSlideParameter);
   end;
   BeRoXMEffectMultiRetrig:begin
    if MultiRetrigOnTick<>0 then begin
     if (Module.Tick mod MultiRetrigOnTick)=0 then begin
      ProcessTrigger(Note);
      case MultiRetrigAction of
       $1:dec(Volume,1);
       $2:dec(Volume,2);
       $3:dec(Volume,4);
       $4:dec(Volume,8);
       $5:dec(Volume,16);
       $6:Volume:=(Volume*2) div 3;
       $7:Volume:=Volume shr 1;
       $9:inc(Volume,1);
       $a:inc(Volume,2);
       $b:inc(Volume,4);
       $c:inc(Volume,8);
       $d:inc(Volume,16);
       $e:Volume:=(Volume*3) shr 1;
       $f:Volume:=Volume*2;
      end;
     end;
    end;
   end;
   BeRoXMEffectTremor:begin
    UpdateTremor;
   end;
   BeRoXMEffectMacro,BeRoXMEffectSmoothMacro:begin
{$ifdef UseMIDIMacros}
    UpdateMacro(Note.Effect,Note.EffectParameter);
{$endif}
   end;
  end;
 end;
end;

procedure TBeRoXMChannel.ProcessTrigger(const Note:TBeRoXMPatternNote);
var TonePorta,DoStartClickRemovalFadeOut,DoNewInstrument,DoNewNote:boolean;
    NewInstrument:TBeRoXMInstrument;
    TargetAudioProcessChain:TBeRoXMAudioProcessChain;
begin
 if not IsVirtualChannel then begin
  TonePorta:=(Note.Effect in [BeRoXMEffectTonePorta,BeRoXMEffectTonePortaVolumeSlide]) or (Note.Volume in [$f0..$ff]);

  DoNewInstrument:=Note.Instrument in [1..255];
  DoNewNote:=Note.Note in [1..96];

  if DoNewInstrument then begin
   DoStartClickRemovalFadeOut:=assigned(Module.Instrument[Note.Instrument]);
  end else begin
   if DoNewInstrument then begin
    NewInstrument:=Module.Instrument[Note.Instrument];
   end else begin
    NewInstrument:=Instrument;
   end;
   DoStartClickRemovalFadeOut:=assigned(NewInstrument) and DoNewNote and ((TonePorta and not assigned(Sample)) or not TonePorta);
  end;
  if DoStartClickRemovalFadeOut then begin
   StartClickRemovalFadeOut;
  end;

  if DoNewInstrument then begin
   Reset;
   Instrument:=Module.Instrument[Note.Instrument];
   if assigned(Instrument) then begin
    VolumeEnvelope.Envelope:=Instrument.VolumeEnvelope;
    PanningEnvelope.Envelope:=Instrument.PanningEnvelope;
    VolumeEnvelope.Reset(0);
    PanningEnvelope.Reset(0);
    VolumeEnvelope.KeyOn:=true;
    PanningEnvelope.KeyOn:=true;
    KeyOff:=false;
    FadeOut:=65536;
   end;
  end;

  if assigned(Instrument) and DoNewNote then begin
   if (TonePorta and not assigned(Sample)) or not TonePorta then begin
    // This IF statement is a special XM hack: The original FastTracker 2.xx
    // doesn't change the sample in this case
    Sample:=Instrument.Sample[Instrument.SampleMap[Note.Note]];
   end;
   if assigned(Sample) then begin
    RealNote:=Note.Note+Sample.RelativeNote-1;
    FineTune:=Sample.FineTune;
    DestPeriod:=Module.GetPeriod(RealNote,FineTune);
    if (Period=0) or not TonePorta then begin
     if LocalFilterMode then FilterActive:=false;
     VolumeEnvelope.Reset(0);
     PanningEnvelope.Reset(0);
     Period:=DestPeriod;
     SamplePosition:=0;
     SampleBackwards:=false;
     if not DoStartClickRemovalFadeOut then begin
      if (AudioProcessChain>0) and (assigned(Module.AudioProcessChains[AudioProcessChain]) and Module.AudioProcessChains[AudioProcessChain].Active) then begin
       TargetAudioProcessChain:=Module.AudioProcessChains[AudioProcessChain];
       TargetAudioProcessChain.LastLeft:=TargetAudioProcessChain.LastLeft+LastLeft;
       TargetAudioProcessChain.LastRight:=TargetAudioProcessChain.LastRight+LastRight;
      end else begin
       Module.LastLeft:=Module.LastLeft+LastLeft;
       Module.LastRight:=Module.LastRight+LastRight;
      end;
      LastLeft:=0;
      LastRight:=0;
     end;
     VolumeLeft:=0;
     VolumeRight:=0;
     VolumeRampingLeft:=0;
     VolumeRampingRight:=0;
     Active:=true;
    end else begin
     RealNote:=Note.Note;
    end;
   end;
  end;
  if DoNewInstrument then begin
   if assigned(Sample) then begin
    Active:=true;
    Volume:=Sample.Volume;
    if Sample.DoPanning then Panning:=Sample.Panning;
   end;
  end;
  if Note.Note=97 then begin
   VolumeEnvelope.KeyOn:=false;
   PanningEnvelope.KeyOn:=false;
   KeyOff:=true;
  end;
 end;
end;

{$ifdef UseFilters}
{$ifndef UseFilterCoefTables}
function TBeRoXMChannel.CutOffToFrequency(CutOffValue,FilterModifierValue:integer):integer;
var FC:single;
    TheFrequency:integer;
begin
 if CutOffValue<0 then CutOffValue:=0;
 if CutOffValue>$7f then CutOffValue:=$7f;
 if Module.ExtendedFilterRange then begin
  FC:=110*pow(2,0.25+((CutOffValue*(FilterModifierValue+256)))/(20*512));
 end else begin
  FC:=110*pow(2,0.25+((CutOffValue*(FilterModifierValue+256)))/(24*512));
 end;
 TheFrequency:=round(FC);
 if TheFrequency<120 then begin
  result:=120;
 end else if TheFrequency>20000 then begin
  result:=20000;
 end else if (TheFrequency*2)>Module.SampleRate then begin
{$ifdef HasSAR}
  result:=SARLongint(Module.SampleRate,1);
{$else}
{$ifdef UseSAR}
  result:=sar(Module.SampleRate,1);
{$else}
  result:=Module.SampleRate div 2;
{$endif}
{$endif}
 end else begin
  result:=TheFrequency;
 end;
end;
{$endif}

procedure TBeRoXMChannel.SetCutOffFilter(FilterReset:boolean);
{$ifndef UseFilterCoefTables}
var FC,FS,FG,FB0,FB1,DMPFAC,D,E,DIW:{$ifdef cpuarm}single{$else}double{$endif};
{$endif}
begin
 if CutOff<0 then begin
  CutOff:=0;
 end else if CutOff>$7f then begin
  CutOff:=$7f;
 end;
 if Resonance<0 then begin
  Resonance:=0;
 end else if Resonance>$7f then begin
  Resonance:=$7f;
 end;
{$ifdef UseFilterCoefTables}
 case FilterMode of
  BeRoXMCutOffLowPass:begin
   with Module.Owner.FilterCoefs[Resonance,CutOff] do begin
    FilterA0:=A0;
    FilterB0:=B0;
    FilterB1:=B1;
   end;
  end;
  BeRoXMCutOffHighPass:begin
   with Module.Owner.FilterCoefs[Resonance,CutOff] do begin
    FilterA0:=BeRoXMFilterLength-A0;
    FilterB0:=B0;
    FilterB1:=B1;
   end;
  end;
 end;
{$else}
 FC:=CutOffToFrequency(CutOff,256);
 FS:=Module.SampleRate;
 FC:=FC*((2*3.14159265358)/FS);
 DMPFAC:=2*pow(10,-((24/128)*Resonance)/20);
 D:=(1-DMPFAC)*FC;
 if D>2 then D:=2;
 D:=(DMPFAC-D)/FC;
 E:=1/(FC*FC);
 DIW:=(1+D+E);
 FG:=1/DIW;
 FB0:=(D+(E*2))/DIW;
 FB1:=(-E)/DIW;
 case FilterMode of
  BeRoXMCutOffLowPass:begin
   FilterA0:=round(FG*BeRoXMFilterLength);
   FilterB0:=round(FB0*BeRoXMFilterLength);
   FilterB1:=round(FB1*BeRoXMFilterLength);
  end;
  BeRoXMCutOffHighPass:begin
   FilterA0:=round((1-FG)*BeRoXMFilterLength);
   FilterB0:=round(FB0*BeRoXMFilterLength);
   FilterB1:=round(FB1*BeRoXMFilterLength);
  end;
 end;
{$endif}
 if FilterReset then begin
  FilterY1:=0;
  FilterY2:=0;
  FilterY3:=0;
  FilterY4:=0;
 end;
 FilterActive:=true;
end;
{$endif}

{$ifdef UseMIDIMacros}
procedure TBeRoXMChannel.ProcessMacro(var MIDIString:TBeRoXMMIDIString;Parameter:integer;Smooth:boolean);
var Macro:TBeRoXMMIDIString;
    MacroValue:longword;
    Position:integer;
    ParameterValue:integer;
    OldCutOffValue:integer;
    InternalCode:longword;
    C:char;
begin
 Macro:=MIDIString;
 MacroValue:=SwapDWordLittleEndian(ptruint(pointer(@Macro)^)) and $7f5f7f5f;
 if MacroValue=$30463046 then begin
  Position:=4;
  InternalCode:=$ffffffff;
  C:=Macro[Position];
  if C in ['0'..'9'] then begin
   InternalCode:=byte(byte(C)-byte('0')) shl 4;
  end else if C in ['A'..'F'] then begin
   InternalCode:=byte(byte(C)-byte('A')+10) shl 4;
  end;
  C:=Macro[Position+1];
  if C in ['0'..'9'] then begin
   InternalCode:=InternalCode+byte(byte(C)-byte('0'));
  end else if C in ['A'..'F'] then begin
   InternalCode:=InternalCode+byte(byte(C)-byte('A')+10);
  end;
  if InternalCode<>$ffffffff then begin
   ParameterValue:=0;
   if Macro[Position+2] in ['z','Z'] then begin
    ParameterValue:=Parameter;
   end else begin
    C:=Macro[Position+2];
    if C in ['0'..'9'] then begin
     ParameterValue:=byte(byte(C)-byte('0')) shl 4;
    end else if C in ['A'..'F'] then begin
     ParameterValue:=byte(byte(C)-byte('A')+10) shl 4;
    end;
    C:=Macro[Position+3];
    if C in ['0'..'9'] then begin
     ParameterValue:=ParameterValue+byte(byte(C)-byte('0'));
    end else if C in ['A'..'F'] then begin
     ParameterValue:=ParameterValue+byte(byte(C)-byte('A')+10);
    end;
   end;
   case InternalCode of
    $00:begin
     OldCutOffValue:=CutOff;
     if ParameterValue<$80 then begin
      if Smooth then begin
       if Tick=0 then begin
        SmoothInitialValue:=CutOff;
        SmoothValueStep:=(ParameterValue-SmoothInitialValue)/Module.Speed;
       end;
       CutOff:=round(SmoothInitialValue+(SmoothValueStep*(Tick+1)));
      end else begin
       CutOff:=ParameterValue;
      end;
     end;
     dec(OldCutOffValue,CutOff);
     if OldCutOffValue<0 then OldCutOffValue:=-OldCutOffValue;
     if (Volume>0) or (OldCutOffValue<$10) or (not FilterActive) or
        (VolumeRampingLeft=0) or ((VolumeRampingRight=0) and (Module.Owner.OutputChannels=2)) then begin
      SetCutOffFilter(not FilterActive);
     end;
    end;
    $01:begin
     if ParameterValue<$80 then begin
      if Smooth then begin
       if Tick=0 then begin
        SmoothInitialValue:=Resonance;
        SmoothValueStep:=(ParameterValue-SmoothInitialValue)/Module.Speed;
       end;
       Resonance:=round(SmoothInitialValue+(SmoothValueStep*(Tick+1)));
      end else begin
       Resonance:=ParameterValue;
      end;
     end;
     SetCutOffFilter(not FilterActive);
    end;
    $02:begin
     if ParameterValue<$20 then begin
      FilterMode:=ParameterValue div (1 shl 4);
      SetCutOffFilter(not FilterActive);
     end;
    end;
   end;
  end;
 end;
end;
{$endif}

constructor TBeRoXMPattern.Create;
begin
 inherited Create;
 Pattern:=nil;
 Channels:=0;
 Rows:=0;
 Clear;
end;

destructor TBeRoXMPattern.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TBeRoXMPattern.Assign(Source:TBeRoXMPattern);
begin
 if assigned(Source) then begin
  Rows:=Source.Rows;
  Channels:=Source.Channels;
  Pattern:=copy(Source.Pattern,0,length(Source.Pattern));
 end;
end;

procedure TBeRoXMPattern.Clear;
begin
 Name:='';
 if assigned(Pattern) then begin
  setlength(Pattern,0);
  Pattern:=nil;
 end;
end;

procedure TBeRoXMPattern.ClearPattern;
var Row,Channel:integer;
    EmptyNote:TBeRoXMPatternNote;
begin
 FillChar(EmptyNote,sizeof(TBeRoXMPatternNote),#0);
 for Row:=0 to Rows-1 do begin
  for Channel:=0 to Channels-1 do begin
   Pattern[(Row*Channels)+Channel]:=EmptyNote;
  end;
 end;
end;

function TBeRoXMPattern.GetNote(Row,Channel:integer):TBeRoXMPatternNote;
var Cell:integer;
begin
 FillChar(result,sizeof(TBeRoXMPatternNote),#0);
 Cell:=(Row*Channels)+Channel;
 if Cell<length(Pattern) then result:=Pattern[Cell];
end;

procedure TBeRoXMPattern.SetNote(Row,Channel:integer;Note:TBeRoXMPatternNote);
var Cell:integer;
begin
 Cell:=(Row*Channels)+Channel;
 if Cell<length(Pattern) then Pattern[Cell]:=Note;
end;

procedure TBeRoXMPattern.Resize(NewRows:integer;NewChannels:byte);
var OldRows,OldChannels,Row,Channel:integer;
    OldPattern:TBeRoXMPatternNotes;
    EmptyNote:TBeRoXMPatternNote;
begin
 OldRows:=Rows;
 OldChannels:=Channels;
 Rows:=NewRows;
 Channels:=NewChannels;
 if (Rows>0) and (Channels>0) then begin
  OldPattern:=copy(Pattern,0,length(Pattern));
  setlength(Pattern,(Rows+1)*Channels);
  FillChar(EmptyNote,sizeof(TBeRoXMPatternNote),#0);
  for Row:=0 to Rows-1 do begin
   for Channel:=0 to Channels-1 do begin
    if Channel<OldChannels then begin
     if Row<OldRows then begin
      if ((Row*OldChannels)+Channel)<length(OldPattern) then begin
       Pattern[(Row*Channels)+Channel]:=OldPattern[(Row*OldChannels)+Channel];
      end else begin
       Pattern[(Row*Channels)+Channel]:=EmptyNote;
      end;
     end else begin
      Pattern[(Row*Channels)+Channel]:=EmptyNote;
     end;
    end else begin
     Pattern[(Row*Channels)+Channel]:=EmptyNote;
    end;
   end;
  end;
  setlength(OldPattern,0);
 end else if assigned(Pattern) then begin
  setlength(Pattern,0);
 end;
end;

function TBeRoXMPattern.IsEmpty:boolean;
var Row,Channel:integer;
    Note:TBeRoXMPatternNote;
begin
 result:=true;
 for Row:=0 to Rows-1 do begin
  for Channel:=0 to Channels-1 do begin
   Note:=Pattern[(Row*Channels)+Channel];
   if (Note.Note<>0) or (Note.Instrument<>0) or (Note.Volume<>0) or
      (Note.Effect<>0) or (Note.EffectParameter<>0) then begin
    result:=false;
    exit;
   end;
  end;
 end;
end;

constructor TBeRoXMModule.Create(AOwner:TBeRoXM);
var Counter:integer;
begin
 inherited Create;
 Owner:=AOwner;
 MasterVolume:=128;
 RampSamples:=BeRoXMRampSamples;
 FastRampSamples:=BeRoXMFastRampSamples;
 for Counter:=1 to BeRoXMLastInstrument do begin
  Instrument[Counter]:=nil;
 end;
 for Counter:=0 to BeRoXMLastChannel do begin
  Channel[Counter]:=TBeRoXMChannel.Create(self,Counter,false,false);
  NewNoteActionChannel[Counter]:=TBeRoXMChannel.Create(self,Counter,true,false);
  ClickRemovalFadeOutChannel[Counter]:=TBeRoXMChannel.Create(self,Counter,true,true);
 end;
 for Counter:=0 to BeRoXMLastPattern do begin
  Pattern[Counter]:=TBeRoXMPattern.Create;
 end;
 EQFXData:=TBeRoXMMemoryStream.Create;
 ModularData:=TBeRoXMMemoryStream.Create;
 for Counter:=0 to 99 do begin
  FXData[Counter]:=TBeRoXMMemoryStream.Create;
 end;
 FillChar(AudioProcessChains,sizeof(TBeRoXMAudioProcessChains),#0);
 Clear;
end;

destructor TBeRoXMModule.Destroy;
var Counter:integer;
begin
 Clear;
 for Counter:=low(TBeRoXMAudioProcessChains) to high(TBeRoXMAudioProcessChains) do begin
  if assigned(AudioProcessChains[Counter]) then begin
   AudioProcessChains[Counter].Destroy;
  end;
 end;
 EQFXData.Destroy;
 ModularData.Destroy;
 for Counter:=0 to 99 do begin
  FXData[Counter].Destroy;
 end;
 for Counter:=0 to BeRoXMLastChannel do begin
  Channel[Counter].Destroy;
  NewNoteActionChannel[Counter].Destroy;
  ClickRemovalFadeOutChannel[Counter].Destroy;
 end;
 for Counter:=0 to BeRoXMLastPattern do begin
  Pattern[Counter].Destroy;
 end;
 inherited Destroy;
end;

procedure TBeRoXMModule.Clear;
var Counter:integer;
begin
 GlobalVolume:=64;
 LastLeft:=0;
 LastRight:=0;
 Name:='';
 Comment:='';
 for Counter:=1 to BeRoXMLastInstrument do begin
  if assigned(Instrument[Counter]) then begin
   Instrument[Counter].Destroy;
   Instrument[Counter]:=nil;
  end;
 end;
 for Counter:=0 to BeRoXMLastPattern do begin
  Pattern[Counter].Clear;
 end;
 for Counter:=0 to BeRoXMLastChannel do begin
  Channel[Counter].Plugin:=0;
  Channel[Counter].AudioProcessChain:=0;
  NewNoteActionChannel[Counter].Plugin:=0;
  NewNoteActionChannel[Counter].AudioProcessChain:=0;
  ClickRemovalFadeOutChannel[Counter].Plugin:=0;
  ClickRemovalFadeOutChannel[Counter].AudioProcessChain:=0;
 end;
 EQFXData.Clear;
 ModularData.Clear;
 for Counter:=0 to 99 do begin
  FXData[Counter].Clear;
 end;
 for Counter:=low(TBeRoXMAudioProcessChains) to high(TBeRoXMAudioProcessChains) do begin
  if assigned(AudioProcessChains[Counter]) then begin
   AudioProcessChains[Counter].Destroy;
   AudioProcessChains[Counter]:=nil;
  end;
 end;
 FillChar(PatternOrder,sizeof(TBeRoXMPatternOrder),#0);
 StartSpeed:=6;
 StartTempo:=125;
 TrackLength:=0;
 RestartPosition:=0;
 LinearSlides:=true;
 ExtendedFilterRange:=false;
 Reset;
 IsTrackActive:=false;
{$ifdef UseMIDIMacros}
 ResetMIDIConfig;
{$endif}
end;

procedure TBeRoXMModule.Reset;
var Counter:integer;
begin
 for Counter:=low(TBeRoXMAudioProcessChains) to high(TBeRoXMAudioProcessChains) do begin
  if assigned(AudioProcessChains[Counter]) then begin
   AudioProcessChains[Counter].Reset;
  end;
 end;

 for Counter:=0 to BeRoXMLastChannel do begin
  Channel[Counter].Clear;
  NewNoteActionChannel[Counter].Clear;
  ClickRemovalFadeOutChannel[Counter].Clear;
 end;

 Speed:=StartSpeed;
 Tempo:=StartTempo;
 Tick:=Speed;

 PatternDelay:=0;

 TrackEnd:=false;

 CurrentPatternOrder:=0;
 CurrentPatternRow:=0;

 NextPatternOrder:=0;
 NextPatternRow:=1;
end;

procedure TBeRoXMModule.ProcessTick;
var Counter:integer;
begin
 inc(Tick);
 if Tick>=(Speed*(PatternDelay+1)) then begin
  Tick:=0;
  PatternDelay:=0;
  NextPatternOrder:=0;
  NextPatternRow:=0;
  Jump:=false;
  while true do begin
   if CurrentPatternOrder>=TrackLength then begin
    if Owner.Looping then begin
     TrackEnd:=false;
     CurrentPatternOrder:=RestartPosition;
     CurrentPatternRow:=0;
     if CurrentPatternOrder=0 then begin
      GlobalVolume:=64;
      Speed:=StartSpeed;
      Tempo:=StartTempo;
      for Counter:=0 to NumberOfChannels-1 do begin
       Channel[Counter].Clear;
      end;
{$ifdef android}
      Owner.OutputSamples:=0;
{$endif}
     end;
    end else begin
     TrackEnd:=true;
    end;
   end;
   CurrentPattern:=PatternOrder[CurrentPatternOrder];
   if CurrentPatternRow>=Pattern[CurrentPattern].Rows then begin
    inc(CurrentPatternOrder);
    CurrentPatternRow:=0;
   end else begin
    break;
   end;
  end;
  CurrentPattern:=PatternOrder[CurrentPatternOrder];
  for Counter:=0 to NumberOfChannels-1 do begin
   Channel[Counter].Tick:=Tick;
   if TrackEnd then begin
    FillChar(Channel[Counter].CurrentNote,sizeof(TBeRoXMPatternNote),#0);
   end else begin         
    Channel[Counter].CurrentNote:=Pattern[CurrentPattern].GetNote(CurrentPatternRow,Counter);
   end;
   Channel[Counter].ProcessRow(Channel[Counter].CurrentNote);
  end;
  if Jump then begin
   CurrentPatternOrder:=NextPatternOrder;
   CurrentPatternRow:=NextPatternRow;
  end else begin
   inc(CurrentPatternRow);
  end;
 end else begin
  for Counter:=0 to NumberOfChannels-1 do begin
   Channel[Counter].Tick:=Tick;
   Channel[Counter].ProcessTick(Channel[Counter].CurrentNote);
  end;
  for Counter:=0 to BeRoXMLastChannel do begin
   NewNoteActionChannel[Counter].Tick:=Tick;
   NewNoteActionChannel[Counter].ProcessTick(NewNoteActionChannel[Counter].CurrentNote);
  end;
  for Counter:=0 to BeRoXMLastChannel do begin
   ClickRemovalFadeOutChannel[Counter].Tick:=Tick;
   ClickRemovalFadeOutChannel[Counter].ProcessTick(ClickRemovalFadeOutChannel[Counter].CurrentNote);
  end;
 end;
 for Counter:=0 to NumberOfChannels-1 do begin
  Channel[Counter].UpdateTick;
 end;
 for Counter:=0 to BeRoXMLastChannel do begin
  NewNoteActionChannel[Counter].UpdateTick;
 end;
 for Counter:=0 to BeRoXMLastChannel do begin
  ClickRemovalFadeOutChannel[Counter].UpdateTick;
 end;
end;

function TBeRoXMModule.GetPeriod(Note,FineTune:integer):integer;
{$ifdef UseTables}
var RealNote,RealOctave,FineTuneValue,PeriodIndex,PeriodA,PeriodB:integer;
{$endif}
begin
 if Note<1 then Note:=1;
 if Note>132 then Note:=132;
 if LinearSlides then begin
{$ifdef HasSAR}
  result:=(10*12*16*4)-(Note*16*4)-SARLongint(FineTune,1);
{$else}
{$ifdef UseSAR}
  result:=(10*12*16*4)-(Note*16*4)-sar(FineTune,1);
{$else}
  result:=(10*12*16*4)-(Note*16*4)-(FineTune div 2);
{$endif}
{$endif}
  if result<1 then result:=1;
 end else begin
{$ifdef UseTables}
  RealNote:=(Note mod 12)*(1 shl 3);
  RealOctave:=Note div 12;
{$ifdef HasSAR}
  FineTuneValue:=SARLongint(FineTune,4);
{$else}
{$ifdef UseSAR}
  FineTuneValue:=sar(FineTune,4);
{$else}
  FineTuneValue:=FineTune div 16;
{$endif}
{$endif}
  PeriodIndex:=RealNote+FineTuneValue+8;
  if PeriodIndex<0 then begin
   PeriodIndex:=0;
  end else if PeriodIndex>=104 then begin
   PeriodIndex:=103;
  end;
  PeriodA:=XMPeriodTable[PeriodIndex];
  if FineTune<0 then begin
   dec(FineTuneValue);
   FineTune:=-FineTune;
  end else begin
   inc(FineTuneValue);
  end;
  PeriodIndex:=RealNote+FineTuneValue+8;
  if PeriodIndex<0 then begin
   PeriodIndex:=0;
  end else if PeriodIndex>=104 then begin
   PeriodIndex:=103;
  end;
  PeriodB:=XMPeriodTable[PeriodIndex];
  FineTuneValue:=FineTune and $0f;
  PeriodA:=PeriodA*(16-FineTuneValue);
  PeriodB:=PeriodB*FineTuneValue;
  result:=sar((PeriodA+PeriodB)*2,RealOctave);
{$else}
  result:=trunc(pow(2,(133-(Note+(FineTune/128)))/12)*13.375);
{$endif}
 end;
end;

function TBeRoXMModule.GetNote(Period,FineTune:integer):integer;
var Counter,Value:integer;
begin    
 result:=120;
 for Counter:=1 to 120-1 do begin
  Value:=GetPeriod(Counter,FineTune);
  if (Value>0) and (Value<=Period) then begin
   result:=Counter;
   exit;
  end;
 end;
end;

function TBeRoXMModule.GetFrequency(Period:integer):integer;
begin
 if LinearSlides then begin
{$ifdef UseTables}
  result:=XMLinearTable[Period mod 768] shr (Period div 768);
{$else}
  result:=round((8363*pow(2,(((6*12*16*4)-Period)/(12*16*4)))));
{$endif}
 end else if Period<>0 then begin
  result:=(8863*1712) div Period;
 end else begin
  result:=1;
 end;
end;

procedure TBeRoXMModule.SetMIDIString(var MIDIString:TBeRoXMMIDIString;S:string);
begin
 if length(S)>32 then begin
  MOVE(S[1],MIDIString,32);
 end else begin
  FillChar(MIDIString,sizeof(TBeRoXMMIDIString),#0);
  if length(S)>0 then begin
   MOVE(S[1],MIDIString,length(S));
  end;
 end;
end;

{$ifdef UseMIDIMacros}
procedure TBeRoXMModule.ResetMIDIConfig;
var Counter:byte;
begin
 FillChar(MIDIConfig,sizeof(TBeRoXMMIDIConfig),#0);
 SetMIDIString(MIDIConfig.Global[MIDIOUT_START],'FF');
 SetMIDIString(MIDIConfig.Global[MIDIOUT_STOP],'FC');
 SetMIDIString(MIDIConfig.Global[MIDIOUT_NOTEON],'9c n v');
 SetMIDIString(MIDIConfig.Global[MIDIOUT_NOTEOFF],'9c n 0');
 SetMIDIString(MIDIConfig.Global[MIDIOUT_PROGRAM],'Cc p');
 SetMIDIString(MIDIConfig.SFXExt[0],'F0F000z');
 for Counter:=$0 to $f do begin
  SetMIDIString(MIDIConfig.ZXXExt[Counter],'F0F001'+Hex2Str(Counter shl 3));
 end;
 EmbeddedMIDIConfig:=false;
end;
{$endif}

function TBeRoXMModule.Load(DataPointer:pointer;DataSize:longword):boolean;
var DataPosition,NextDataPosition:longword;
    Header:TXMHeader;
    EmptyNote:TBeRoXMPatternNote;
    NumberOfPatterns:integer;
    NumberOfInstruments:integer;
    Counter:integer;
    InstrumentSamplesCount:array[0..BeRoXMLastInstrument] of integer;
 function Read(var Buffer;LengthCounter:longword):longword;
 var  RealSize:longword;
 begin
  RealSize:=LengthCounter;
  if (DataPosition+RealSize)>DataSize then RealSize:=DataSize-DataPosition;
  if RealSize<>LengthCounter then FillChar(Buffer,LengthCounter,#0);
  if RealSize>0 then begin
   MOVE(PBeRoXMStreamData(DataPointer)^[DataPosition],Buffer,RealSize);
  end;
  inc(DataPosition,RealSize);
  result:=RealSize;
 end;
 function StreamRead(Stream:TBeRoXMStream;LengthCounter:longword):longword;
 var  RealSize:longword;
 begin
  RealSize:=LengthCounter;
  if (DataPosition+RealSize)>DataSize then RealSize:=DataSize-DataPosition;
  if RealSize>0 then begin
   Stream.Read(PBeRoXMStreamData(DataPointer)^[DataPosition],RealSize);
  end;
  inc(DataPosition,RealSize);
  result:=RealSize;
 end;
 function ReadByte:byte;
 begin
  Read(result,sizeof(byte));
 end;
 function ReadWord:word;
 begin
  Read(result,sizeof(word));
  SwapLittleEndianData16(result);
 end;
 function ReadShortInt:shortint;
 begin
  Read(result,sizeof(shortint));
 end;
 function ReadSmallInt:smallint;
 begin
  Read(result,sizeof(smallint));
  SwapLittleEndianData16(result);
 end;
{$ifdef UseDPCM4}
 procedure ReadDPCM4Sample(Size:longword;Sample:TBeRoXMSample);
 var AByte:byte;
     Delta:shortint;
     Counter,SampleCounter,HalfLength:longword;
     DPCM4Table:TBeRoXMDPCM4Table;
     Buffer:array of shortint;
 begin
  setlength(Buffer,Size);
  Read(DPCM4Table,16);
  HalfLength:=(Size+1) div 2;
  Delta:=0;
  for Counter:=0 to HalfLength-1 do begin
   AByte:=ReadByte;
   Delta:=shortint(byte(byte(Delta)+byte(DPCM4Table[AByte and $f])));
   Buffer[Counter*2]:=Delta;
   Delta:=shortint(byte(byte(Delta)+byte(DPCM4Table[AByte shr 4])));
   Buffer[(Counter*2)+1]:=Delta;
  end;
  Sample.Resize(Size);
  if Sample.Channels=2 then begin
   SampleCounter:=0;
   for Counter:=0 to (Size div 2)-1 do begin
    Sample.Data[SampleCounter]:=Buffer[Counter]*256;
    inc(SampleCounter);
    Sample.Data[SampleCounter]:=Buffer[Counter+(Size div 2)]*256;
    inc(SampleCounter);
   end;
  end else begin
   for Counter:=0 to Size-1 do begin
    Sample.Data[Counter]:=Buffer[Counter]*256;
   end;
  end;
  setlength(Buffer,0);
 end;
{$endif}
 procedure Read8BitSample(Size:longword;Sample:TBeRoXMSample);
 var Counter,SampleCounter:longword;
     Current,Last,New:shortint;
 begin
  Sample.Resize(Size);
  if Sample.Channels=2 then begin
   Last:=0;
   SampleCounter:=0;
   for Counter:=0 to (Size div 2)-1 do begin
    Current:=ReadShortInt;
    New:=Current+Last;
    Sample.Data[SampleCounter]:=New*256;
    inc(SampleCounter,2);
    Last:=New;
   end;
   Last:=0;
   SampleCounter:=1;
   for Counter:=0 to (Size div 2)-1 do begin
    Current:=ReadShortInt;
    New:=Current+Last;
    Sample.Data[SampleCounter]:=New*256;
    inc(SampleCounter,2);
    Last:=New;
   end;
  end else begin
   Last:=0;
   for Counter:=0 to Size-1 do begin
    Current:=ReadShortInt;
    New:=Current+Last;
    Sample.Data[Counter]:=New*256;
    Last:=New;
   end;
  end;
 end;
 procedure Read16BitSample(Size:longword;Sample:TBeRoXMSample);
 var Counter,SampleCounter:longword;
     Current,Last,New:smallint;
 begin
  Sample.Resize(Size div 2);
  if Sample.Channels=2 then begin
   Last:=0;
   SampleCounter:=0;
   for Counter:=0 to (Size div 4)-1 do begin
    Current:=ReadSmallInt;
    New:=Current+Last;
    Sample.Data[SampleCounter]:=New;
    inc(SampleCounter,2);
    Last:=New;
   end;
   Last:=0;
   SampleCounter:=1;
   for Counter:=0 to (Size div 4)-1 do begin
    Current:=ReadSmallInt;
    New:=Current+Last;
    Sample.Data[SampleCounter]:=New;
    inc(SampleCounter,2);
    Last:=New;
   end;
  end else begin
   Last:=0;
   for Counter:=0 to (Size div 2)-1 do begin
    Current:=ReadSmallInt;
    New:=Current+Last;
    Sample.Data[Counter]:=New;
    Last:=New;
   end;
  end;
 end;
{$ifdef UseAmigaMODLoader}
 function MOD2XMNote(Period:longword):longword;
 const AmigaMODPeriods:array[1..60] of word=(1712,1616,1524,1440,1356,1280,1208,1140,1076,1016,960,906,856,808,762,720,678,640,604,570,538,508,480,453,428,404,381,360,339,320,302,285,269,254,240,226,214,202,190,180,170,160,151,143,135,127,120,113,107,101,
95,90,85,80,75,71,67,63,60,56);
 var Note:longword;
 begin
  result:=0;
  if Period>0 then begin
   for Note:=0 to 60 do begin
    if Period>=AmigaMODPeriods[Note] then begin
     result:=Note+24;
     break;
    end;
   end;
  end;
 end;
 function MOD2XMFineTune(FineTune:longint):longint;
 begin
  result:=FineTune;
  if result<0 then result:=0;
  if result>15 then result:=15;
  if result>7 then result:=-(8-(result-8));
  result:=result*16;
 end;
 function LoadMOD:boolean;
 var TotalSampleLength,PatternStartPosition,PatternA,PatternB,
     HowManyChannels,HowManySamples,HowManyPatterns:longword;
     ASample:TMODSample;
     ANote:TBeRoXMPatternNote;
     OldMODFormat:longbool;
     Counter,SampleCounter,RowCounter,ChannelChannel:integer;
     Buffer:array[1..4] of byte;
     S:shortint;
     Header15:TMODHeader15;
     Header31:TMODHeader31;
     Sample:array[0..31] of TMODSample;
     DPCM:array[1..5] of char;
 begin
  result:=false;
  OldMODFormat:=false;

  FillChar(Sample,sizeof(Sample),#0);
  
  HowManyChannels:=0;
  HowManySamples:=31;

  Name:='';

  DataPosition:=0;
  Read(Header31,sizeof(TMODHeader31));

  if Header31.Tag[0]='P' then if Header31.Tag[1]='P' then exit;
  if (Header31.Tag='M.K.') or (Header31.Tag='M!K!') or (Header31.Tag='M&K!') or (Header31.Tag='N.T.') then begin
   HowManyChannels:=4;
  end else if (Header31.Tag='OCTA') or (Header31.Tag='OKTA') or (Header31.Tag='CD81') or (Header31.Tag='WOW!') then begin
   HowManyChannels:=8;
  end else if (Header31.Tag[0]='T') and (Header31.Tag[1]='D') and (Header31.Tag[2]='Z') then begin
   HowManyChannels:=ord(Header31.Tag[3])-48;
  end else if (Header31.Tag[0]='F') and (Header31.Tag[1]='L') and (Header31.Tag[2]='T') then begin
   HowManyChannels:=ord(Header31.Tag[3])-48;
   if not HowManyChannels in [1..8] then HowManyChannels:=0;
  end else if (Header31.Tag[0]='E') and (Header31.Tag[1]='X') and (Header31.Tag[2]='O') then begin
   HowManyChannels:=ord(Header31.Tag[3])-48;
   if not HowManyChannels in [1..8] then HowManyChannels:=0;
  end else if (Header31.Tag[1]='C') and (Header31.Tag[2]='H') and (Header31.Tag[3]='N') then begin
   HowManyChannels:=ord(Header31.Tag[0])-48;
  end else if (Header31.Tag[2]='C') and (Header31.Tag[3]='N') then begin
   HowManyChannels:=((ord(Header31.Tag[0])-48)*10)+(ord(Header31.Tag[1])-48);
  end else if (Header31.Tag[2]='C') and (Header31.Tag[3]='H') then begin
   HowManyChannels:=((ord(Header31.Tag[0])-48)*10)+(ord(Header31.Tag[1])-48);
  end else if (Header31.Tag[0]='E') and (Header31.Tag[1]='X') then begin
   HowManyChannels:=((ord(Header31.Tag[2])-48)*10)+(ord(Header31.Tag[3])-48);
  end;
  if HowManyChannels=0 then begin
   HowManySamples:=15;
   HowManyChannels:=4;
   OldMODFormat:=true;

   DataPosition:=0;
   Read(Header15,sizeof(TMODHeader15));

   Name:=Header15.Title;

   TrackLength:=Header15.TrackLength;
   RestartPosition:=Header15.RestartPosition;

   MOVE(Header15.PatternOrder,PatternOrder,128);
  end else begin
   Name:=Header31.Title;

   TrackLength:=Header31.TrackLength;
   RestartPosition:=Header31.RestartPosition;

   MOVE(Header31.PatternOrder,PatternOrder,128);
  end;

  if RestartPosition>TrackLength then RestartPosition:=0;

  TotalSampleLength:=0;
  for Counter:=1 to HowManySamples do begin
   if OldMODFormat then begin
    ASample:=Header15.Samples[Counter];
   end else begin
    ASample:=Header31.Samples[Counter];
   end;
   Sample[Counter]:=ASample;
   if not assigned(Instrument[Counter]) then Instrument[Counter]:=TBeRoXMInstrument.Create;
   Instrument[Counter].Clear;
   Instrument[Counter].FadeOut:=65536;
   Instrument[Counter].VolumeEnvelope.Active:=false;
   Instrument[Counter].PanningEnvelope.Active:=false;
   Instrument[Counter].VibratoRate:=0;
   if not assigned(Instrument[Counter].Sample[0]) then Instrument[Counter].Sample[0]:=TBeRoXMSample.Create;
   Instrument[Counter].Sample[0].Clear;
   Instrument[Counter].Sample[0].Bits:=8;
   Instrument[Counter].Sample[0].Channels:=1;
   Instrument[Counter].Sample[0].SampleLength:=SwapWordBigEndian(ASample.LengthCounter) shl 1;
   Instrument[Counter].Sample[0].LoopStart:=SwapWordBigEndian(ASample.LoopStart) shl 1;
   Instrument[Counter].Sample[0].LoopLength:=SwapWordBigEndian(ASample.LoopLength) shl 1;
   Instrument[Counter].Sample[0].Loop:=false;
   Instrument[Counter].Sample[0].FineTune:=MOD2XMFineTune(ASample.FineTune);
   Instrument[Counter].Sample[0].Volume:=ASample.Volume;
   Instrument[Counter].Sample[0].Panning:=128;
   Instrument[Counter].Sample[0].DoPanning:=false;
   inc(TotalSampleLength,Instrument[Counter].Sample[0].SampleLength);
  end;

  if OldMODFormat then begin
   PatternStartPosition:=sizeof(TMODHeader15);
  end else begin
   PatternStartPosition:=sizeof(TMODHeader31);
  end;

  PatternB:=0;
  for Counter:=0 to 127 do if PatternOrder[Counter]<=127 then begin
   if PatternOrder[Counter]>PatternB then PatternB:=PatternOrder[Counter];
  end;
  inc(PatternB);

  if HowManyChannels=4 then begin
   if ptruint(PatternStartPosition+(PatternB*8*4*64)+TotalSampleLength)=DataSize then begin
    HowManyChannels:=8;
   end;
  end;

  PatternA:=(ptruint(DataSize)-PatternStartPosition-TotalSampleLength) div ((((1024 div 4)*HowManyChannels) div 64)*64);

  if OldMODFormat or (((PatternA>64) and (PatternB<=64))) then begin
   PatternA:=PatternB;
  end;
  if PatternA=PatternB then begin
   HowManyPatterns:=PatternA;
  end else begin
   if PatternB<PatternA then begin
    HowManyPatterns:=PatternB;
   end else if ((PatternB>64) and ((Header31.Tag<>'M!K!') and not OldMODFormat)) then begin
    HowManyPatterns:=PatternA;
   end else begin
    HowManyPatterns:=PatternB;
   end;
  end;
  if HowManyPatterns>128 then HowManyPatterns:=128;

  if ptruint(PatternStartPosition+(HowManyPatterns*HowManyChannels*4*64)+TotalSampleLength)<>DataSize then begin
   Clear;
   exit;
  end;

  for Counter:=0 to (HowManyChannels div 4)-1 do begin
   Channel[(Counter*4)].StandardPanning:=0;
   Channel[(Counter*4)+1].StandardPanning:=255;
   Channel[(Counter*4)+2].StandardPanning:=255;
   Channel[(Counter*4)+3].StandardPanning:=0;
  end;

  for Counter:=0 to HowManyPatterns-1 do begin
   Pattern[Counter].Resize(64,HowManyChannels);
   for RowCounter:=0 to 63 do begin
    for ChannelChannel:=0 to HowManyChannels-1 do begin
     Read(Buffer,4);
     ANote.Note:=MOD2XMNote(((Buffer[1] and $0f) shl 8) or Buffer[2]);
     ANote.Instrument:=(Buffer[1] and $f0) or (Buffer[3] shr 4);
     ANote.Volume:=0;
     ANote.Effect:=Buffer[3] and $f;
     ANote.EffectParameter:=Buffer[4];
     Pattern[Counter].SetNote(RowCounter,ChannelChannel,ANote);
    end;
   end;
  end;

  for Counter:=1 to HowManySamples do begin
   if Instrument[Counter].Sample[0].SampleLength>1 then begin
    if Instrument[Counter].Sample[0].LoopStart>Instrument[Counter].Sample[0].SampleLength then begin
     Instrument[Counter].Sample[0].LoopStart:=0;
     Instrument[Counter].Sample[0].LoopLength:=0;
    end;
    if (Instrument[Counter].Sample[0].LoopStart+Instrument[Counter].Sample[0].LoopLength)>Instrument[Counter].Sample[0].SampleLength then begin
     Instrument[Counter].Sample[0].LoopLength:=Instrument[Counter].Sample[0].SampleLength-Instrument[Counter].Sample[0].LoopStart;
     if Instrument[Counter].Sample[0].LoopLength<2 then Instrument[Counter].Sample[0].LoopLength:=2;
    end;
    Instrument[Counter].Sample[0].Loop:=Instrument[Counter].Sample[0].LoopLength>2;
    if Instrument[Counter].Sample[0].SampleLength>0 then begin
     Read(DPCM,sizeof(DPCM));
     if DPCM='DPCM' then begin
{$ifdef UseDPCM4}
      ReadDPCM4Sample(Instrument[Counter].Sample[0].SampleLength,Instrument[Counter].Sample[0]);
{$endif}      
     end else begin
      dec(DataPosition,4);
      Instrument[Counter].Sample[0].Resize(Instrument[Counter].Sample[0].SampleLength);
      for SampleCounter:=0 to Instrument[Counter].Sample[0].SampleLength-1 do begin
       Read(S,sizeof(shortint));
       Instrument[Counter].Sample[0].Data^[SampleCounter]:=S*256;
      end;
     end;
     Instrument[Counter].Sample[0].FixUp;
    end;
   end;
  end;

  NumberOfChannels:=HowManyChannels;

  LinearSlides:=false;

  IsTrackActive:=true;
  result:=true;
 end;
{$endif}
 function LoadPatterns:boolean;
 var PatternCounter,RowChannel,ChannelCounter,Rows:integer;
     PackedSize:longword;
     PatternHeader:TXMPatternHeader;
     OldPatternHeader:TXMOldPatternHeader;
     ANote:TBeRoXMPatternNote;
     AByte:byte;
 begin
  for PatternCounter:=0 to NumberOfPatterns-1 do begin
   if Header.Version<=$102 then begin
    if Read(OldPatternHeader,sizeof(TXMOldPatternHeader))<>sizeof(TXMOldPatternHeader) then begin
     Clear;
     result:=false;
     exit;
    end;
    SwapLittleEndianData32(OldPatternHeader.Size);
    SwapLittleEndianData16(OldPatternHeader.PackedSize);
    PackedSize:=OldPatternHeader.PackedSize;
    Rows:=OldPatternHeader.Rows+1;
   end else begin
    if Read(PatternHeader,sizeof(TXMPatternHeader))<>sizeof(TXMPatternHeader) then begin
     Clear;
     result:=false;
     exit;
    end;
    SwapLittleEndianData32(PatternHeader.Size);
    SwapLittleEndianData16(PatternHeader.Rows);
    SwapLittleEndianData16(PatternHeader.PackedSize);
    PackedSize:=PatternHeader.PackedSize;
    Rows:=PatternHeader.Rows;
   end;
   Pattern[PatternCounter].Clear;
   Pattern[PatternCounter].Resize(Rows,NumberOfChannels);
   if PackedSize>0 then begin
    NextDataPosition:=DataPosition+PackedSize;
    if (Rows>256) or (Rows=0) then begin
     DataPosition:=NextDataPosition;
     continue;
    end;
    for RowChannel:=0 to Rows-1 do begin
     for ChannelCounter:=0 to NumberOfChannels-1 do begin
      ANote:=EmptyNote;
      AByte:=ReadByte;
      if (AByte and 128)=0 then begin
       ANote.Note:=AByte;
       ANote.Instrument:=ReadByte;
       ANote.Volume:=ReadByte;
       ANote.Effect:=ReadByte;
       ANote.EffectParameter:=ReadByte;
      end;
      if (AByte and 128)<>0 then begin
       if (AByte and 1)<>0 then ANote.Note:=ReadByte;
       if (AByte and 2)<>0 then ANote.Instrument:=ReadByte;
       if (AByte and 4)<>0 then ANote.Volume:=ReadByte;
       if (AByte and 8)<>0 then ANote.Effect:=ReadByte;
       if (AByte and 16)<>0 then ANote.EffectParameter:=ReadByte;
      end;
      if ANote.Instrument=$ff then ANote.Instrument:=0;
      Pattern[PatternCounter].SetNote(RowChannel,ChannelCounter,ANote);
     end;
    end;
    DataPosition:=NextDataPosition;
   end;
  end;
  result:=true;
 end;
 function LoadInstrumentSamplesData(InstrumentCounter,SamplesCount:integer):boolean;
 var SampleCounter:integer;
 begin
  for SampleCounter:=0 to SamplesCount-1 do begin
   NextDataPosition:=DataPosition;
   inc(NextDataPosition,Instrument[InstrumentCounter].Sample[SampleCounter].SampleLength);
   if Instrument[InstrumentCounter].Sample[SampleCounter].SampleLength>BeRoXMMaxSampleSize then begin
    Instrument[InstrumentCounter].Sample[SampleCounter].SampleLength:=BeRoXMMaxSampleSize;
    if Instrument[InstrumentCounter].Sample[SampleCounter].LoopStart>Instrument[InstrumentCounter].Sample[SampleCounter].SampleLength then begin
     Instrument[InstrumentCounter].Sample[SampleCounter].LoopStart:=0;
     Instrument[InstrumentCounter].Sample[SampleCounter].Loop:=false;
    end;
   end;
   if assigned(Instrument[InstrumentCounter].Sample[SampleCounter]) then begin
    if Instrument[InstrumentCounter].Sample[SampleCounter].SampleLength>0 then begin
{$ifdef UseDPCM4}
     if Instrument[InstrumentCounter].Sample[SampleCounter].DPCM4 then begin
      ReadDPCM4Sample(Instrument[InstrumentCounter].Sample[SampleCounter].SampleLength,Instrument[InstrumentCounter].Sample[SampleCounter]);
     end else{$endif} if Instrument[InstrumentCounter].Sample[SampleCounter].Bits=16 then begin
      Read16BitSample(Instrument[InstrumentCounter].Sample[SampleCounter].SampleLength,Instrument[InstrumentCounter].Sample[SampleCounter]);
      Instrument[InstrumentCounter].Sample[SampleCounter].SampleLength:=Instrument[InstrumentCounter].Sample[SampleCounter].SampleLength div 2;
      Instrument[InstrumentCounter].Sample[SampleCounter].LoopStart:=Instrument[InstrumentCounter].Sample[SampleCounter].LoopStart div 2;
      Instrument[InstrumentCounter].Sample[SampleCounter].LoopLength:=Instrument[InstrumentCounter].Sample[SampleCounter].LoopLength div 2;
     end else begin
      Read8BitSample(Instrument[InstrumentCounter].Sample[SampleCounter].SampleLength,Instrument[InstrumentCounter].Sample[SampleCounter]);
     end;
     if Instrument[InstrumentCounter].Sample[SampleCounter].Channels=2 then begin
      Instrument[InstrumentCounter].Sample[SampleCounter].SampleLength:=Instrument[InstrumentCounter].Sample[SampleCounter].SampleLength div 2;
      Instrument[InstrumentCounter].Sample[SampleCounter].LoopStart:=Instrument[InstrumentCounter].Sample[SampleCounter].LoopStart div 2;
      Instrument[InstrumentCounter].Sample[SampleCounter].LoopLength:=Instrument[InstrumentCounter].Sample[SampleCounter].LoopLength div 2;
     end;
     Instrument[InstrumentCounter].Sample[SampleCounter].FixUp;
    end;
   end;
   DataPosition:=NextDataPosition;
  end;
  result:=true;
 end;
 function LoadInstruments:boolean;
 var InstrumentCounter,SampleCounter,SubCounter:integer;
     InstrumentHeader:TXMInstrumentHeader;
     InstrumentExtraHeader:TXMInstrumentExtraHeader;
     SampleHeader:TXMSampleHeader;
 begin
  for InstrumentCounter:=1 to NumberOfInstruments do begin
   if Read(InstrumentHeader,sizeof(TXMInstrumentHeader))<>sizeof(TXMInstrumentHeader) then begin
    Clear;
    result:=false;
    exit;
   end;
   if not assigned(Instrument[InstrumentCounter]) then begin
    Instrument[InstrumentCounter]:=TBeRoXMInstrument.Create;
   end;
   Instrument[InstrumentCounter].Clear;
   Instrument[InstrumentCounter].Name:=trim(InstrumentHeader.Name);
   SwapLittleEndianData32(InstrumentHeader.Size);
   SwapLittleEndianData16(InstrumentHeader.CountOfSamples);
   if InstrumentHeader.CountOfSamples=0 then begin
    inc(DataPosition,integer(InstrumentHeader.Size)-sizeof(TXMInstrumentHeader));
   end else if InstrumentHeader.CountOfSamples>0 then begin
    if Read(InstrumentExtraHeader,sizeof(TXMInstrumentExtraHeader))<>sizeof(TXMInstrumentExtraHeader) then begin
     Clear;
     result:=false;
     exit;
    end;
    inc(DataPosition,integer(InstrumentHeader.Size)-sizeof(TXMInstrumentHeader)-sizeof(TXMInstrumentExtraHeader));

    SwapLittleEndianData32(InstrumentExtraHeader.Size);
    SwapLittleEndianData16(InstrumentExtraHeader.FadeOut);
    SwapLittleEndianData16(InstrumentExtraHeader.Reserved);

    Instrument[InstrumentCounter].FadeOut:=InstrumentExtraHeader.FadeOut;

    for SubCounter:=1 to 96 do begin
     Instrument[InstrumentCounter].SampleMap[SubCounter]:=InstrumentExtraHeader.SampleMap[SubCounter];
    end;

    for SubCounter:=0 to 12-1 do begin
     Instrument[InstrumentCounter].VolumeEnvelope.Points[SubCounter].Tick:=SwapWordLittleEndian(InstrumentExtraHeader.VolumeEnvelope[SubCounter*2]);
     Instrument[InstrumentCounter].VolumeEnvelope.Points[SubCounter].Value:=(byte(SwapWordLittleEndian(InstrumentExtraHeader.VolumeEnvelope[(SubCounter*2)+1])) and $ff);
     Instrument[InstrumentCounter].PanningEnvelope.Points[SubCounter].Tick:=SwapWordLittleEndian(InstrumentExtraHeader.PanningEnvelope[SubCounter*2]);
     Instrument[InstrumentCounter].PanningEnvelope.Points[SubCounter].Value:=(byte(SwapWordLittleEndian(InstrumentExtraHeader.PanningEnvelope[(SubCounter*2)+1])) and $ff);
     if SubCounter>0 then begin
      if Instrument[InstrumentCounter].VolumeEnvelope.Points[SubCounter].Tick<Instrument[InstrumentCounter].VolumeEnvelope.Points[SubCounter-1].Tick then begin
       Instrument[InstrumentCounter].VolumeEnvelope.Points[SubCounter].Tick:=Instrument[InstrumentCounter].VolumeEnvelope.Points[SubCounter].Tick and $ff;
       inc(Instrument[InstrumentCounter].VolumeEnvelope.Points[SubCounter].Tick,Instrument[InstrumentCounter].VolumeEnvelope.Points[SubCounter-1].Tick and $ff00);
       if Instrument[InstrumentCounter].VolumeEnvelope.Points[SubCounter].Tick<Instrument[InstrumentCounter].VolumeEnvelope.Points[SubCounter-1].Tick then inc(Instrument[InstrumentCounter].VolumeEnvelope.Points[SubCounter].Tick,$100);
      end;
      if Instrument[InstrumentCounter].PanningEnvelope.Points[SubCounter].Tick<Instrument[InstrumentCounter].PanningEnvelope.Points[SubCounter-1].Tick then begin
       Instrument[InstrumentCounter].PanningEnvelope.Points[SubCounter].Tick:=Instrument[InstrumentCounter].PanningEnvelope.Points[SubCounter].Tick and $ff;
       inc(Instrument[InstrumentCounter].PanningEnvelope.Points[SubCounter].Tick,Instrument[InstrumentCounter].PanningEnvelope.Points[SubCounter-1].Tick and $ff00);
       if Instrument[InstrumentCounter].PanningEnvelope.Points[SubCounter].Tick<Instrument[InstrumentCounter].PanningEnvelope.Points[SubCounter-1].Tick then inc(Instrument[InstrumentCounter].PanningEnvelope.Points[SubCounter].Tick,$100);
      end;
     end;
    end;

    Instrument[InstrumentCounter].VolumeEnvelope.NumberOfPoints:=InstrumentExtraHeader.VolumeEnvelopeNumPoints;
    Instrument[InstrumentCounter].VolumeEnvelope.LoopStartPoint:=InstrumentExtraHeader.VolumeEnvelopeLoopStart;
    Instrument[InstrumentCounter].VolumeEnvelope.LoopEndPoint:=InstrumentExtraHeader.VolumeEnvelopeLoopEnd;
    Instrument[InstrumentCounter].VolumeEnvelope.SustainPoint:=InstrumentExtraHeader.VolumeEnvelopeSustain;

    Instrument[InstrumentCounter].PanningEnvelope.NumberOfPoints:=InstrumentExtraHeader.PanningEnvelopeNumPoints;
    Instrument[InstrumentCounter].PanningEnvelope.LoopStartPoint:=InstrumentExtraHeader.PanningEnvelopeLoopStart;
    Instrument[InstrumentCounter].PanningEnvelope.LoopEndPoint:=InstrumentExtraHeader.PanningEnvelopeLoopEnd;
    Instrument[InstrumentCounter].PanningEnvelope.SustainPoint:=InstrumentExtraHeader.PanningEnvelopeSustain;

    Instrument[InstrumentCounter].VolumeEnvelope.Active:=(InstrumentExtraHeader.VolFlags and 1)<>0;
    Instrument[InstrumentCounter].VolumeEnvelope.Sustain:=(InstrumentExtraHeader.VolFlags and 2)<>0;
    Instrument[InstrumentCounter].VolumeEnvelope.Loop:=(InstrumentExtraHeader.VolFlags and 4)<>0;
    Instrument[InstrumentCounter].PanningEnvelope.Active:=(InstrumentExtraHeader.PanFlags and 1)<>0;
    Instrument[InstrumentCounter].PanningEnvelope.Sustain:=(InstrumentExtraHeader.PanFlags and 2)<>0;
    Instrument[InstrumentCounter].PanningEnvelope.Loop:=(InstrumentExtraHeader.PanFlags and 4)<>0;

    if Instrument[InstrumentCounter].VolumeEnvelope.NumberOfPoints=0 then Instrument[InstrumentCounter].VolumeEnvelope.Active:=false;
    if Instrument[InstrumentCounter].PanningEnvelope.NumberOfPoints=0 then Instrument[InstrumentCounter].PanningEnvelope.Active:=false;
    if Instrument[InstrumentCounter].VolumeEnvelope.SustainPoint>12 then Instrument[InstrumentCounter].VolumeEnvelope.Sustain:=false;
    if Instrument[InstrumentCounter].PanningEnvelope.SustainPoint>12 then Instrument[InstrumentCounter].PanningEnvelope.Sustain:=false;
    if Instrument[InstrumentCounter].VolumeEnvelope.LoopStartPoint>=Instrument[InstrumentCounter].VolumeEnvelope.LoopEndPoint then Instrument[InstrumentCounter].VolumeEnvelope.Loop:=false;
    if Instrument[InstrumentCounter].PanningEnvelope.LoopStartPoint>=Instrument[InstrumentCounter].PanningEnvelope.LoopEndPoint then Instrument[InstrumentCounter].PanningEnvelope.Loop:=false;

    Instrument[InstrumentCounter].VibratoType:=InstrumentExtraHeader.VibratoType;
    Instrument[InstrumentCounter].VibratoSweep:=InstrumentExtraHeader.VibratoSweep;
    Instrument[InstrumentCounter].VibratoDepth:=InstrumentExtraHeader.VibratoDepth;
    Instrument[InstrumentCounter].VibratoRate:=InstrumentExtraHeader.VibratoRate;

    for SampleCounter:=0 to InstrumentHeader.CountOfSamples-1 do begin
     if Read(SampleHeader,sizeof(SampleHeader))<>sizeof(SampleHeader) then begin
      Clear;
      result:=false;
      exit;
     end;
     SwapLittleEndianData32(SampleHeader.SampleLength);
     SwapLittleEndianData32(SampleHeader.LoopStart);
     SwapLittleEndianData32(SampleHeader.LoopLength);
     if SampleHeader.LoopStart>=SampleHeader.SampleLength then SampleHeader.Flags:=SampleHeader.Flags and not 3;
     if (SampleHeader.LoopStart+SampleHeader.LoopLength)>SampleHeader.SampleLength then begin
      SampleHeader.LoopLength:=SampleHeader.SampleLength-SampleHeader.LoopStart;
     end;
     if SampleHeader.LoopLength<=0 then SampleHeader.Flags:=SampleHeader.Flags and not 3;
     if not assigned(Instrument[InstrumentCounter].Sample[SampleCounter]) then begin
      Instrument[InstrumentCounter].Sample[SampleCounter]:=TBeRoXMSample.Create;
     end;
     Instrument[InstrumentCounter].Sample[SampleCounter].Clear;
     Instrument[InstrumentCounter].Sample[SampleCounter].Name:=trim(SampleHeader.Name);
     Instrument[InstrumentCounter].Sample[SampleCounter].SampleLength:=SampleHeader.SampleLength;
     Instrument[InstrumentCounter].Sample[SampleCounter].LoopStart:=SampleHeader.LoopStart;
     Instrument[InstrumentCounter].Sample[SampleCounter].LoopLength:=SampleHeader.LoopLength;
     Instrument[InstrumentCounter].Sample[SampleCounter].Volume:=SampleHeader.Volume;
     Instrument[InstrumentCounter].Sample[SampleCounter].FineTune:=SampleHeader.FineTune;
     Instrument[InstrumentCounter].Sample[SampleCounter].RelativeNote:=SampleHeader.RelativeNote;
     Instrument[InstrumentCounter].Sample[SampleCounter].Panning:=SampleHeader.Panning;
     Instrument[InstrumentCounter].Sample[SampleCounter].DoPanning:=true;
     Instrument[InstrumentCounter].Sample[SampleCounter].Bits:=8+(8*ord((SampleHeader.Flags and 16)<>0));
     Instrument[InstrumentCounter].Sample[SampleCounter].Channels:=1+ord((SampleHeader.Flags and 32)<>0);
{$ifdef UseDPCM4}
     Instrument[InstrumentCounter].Sample[SampleCounter].DPCM4:=(SampleHeader.Reserved=$ad) and ((SampleHeader.Flags and $30)=0);
{$endif}
     Instrument[InstrumentCounter].Sample[SampleCounter].Loop:=(SampleHeader.Flags and (1 or 2))<>0;
     Instrument[InstrumentCounter].Sample[SampleCounter].PingPongLoop:=(SampleHeader.Flags and 2)<>0;
    end;
    if Header.Version<=$103 then begin
     InstrumentSamplesCount[InstrumentCounter]:=InstrumentHeader.CountOfSamples;
    end else begin
     LoadInstrumentSamplesData(InstrumentCounter,InstrumentHeader.CountOfSamples);
    end;
   end;
  end;
  result:=true;
 end;
 function LoadInstrumentSamples:boolean;
 var InstrumentCounter:integer;
 begin
  result:=false;
  for InstrumentCounter:=1 to NumberOfInstruments do begin
   result:=LoadInstrumentSamplesData(InstrumentCounter,InstrumentSamplesCount[InstrumentCounter]);
   if not result then exit;
  end;
 end;
 function LoadExtendedData:boolean;
 var Counter,ItemSize:integer;
     LastPosition,BlockLastPosition,BlockSize,Value:longword;
     Signature:TBeRoXMChunkSignature;
     C32:array[0..32-1] of char;
     CommentChar:char;
 begin
  LastPosition:=DataPosition;
  while (DataPosition+8)<DataSize do begin
   BlockLastPosition:=DataPosition;
   if Read(Signature,sizeof(TBeRoXMChunkSignature))<>sizeof(TBeRoXMChunkSignature) then begin
    result:=false;
    exit;
   end;
   if Read(BlockSize,sizeof(longword))<>sizeof(longword) then begin
    result:=false;
    exit;
   end;
   SwapLittleEndianData32(BlockSize);
   if Signature='text' then begin
    Comment:='';
    for Counter:=0 to BlockSize-1 do begin
     if Read(CommentChar,1)<>1 then begin
      result:=false;
      exit;
     end;
     Comment:=Comment+CommentChar;
     if CommentChar=#13 then Comment:=Comment+#10;
    end;
{$ifdef UseMIDIMacros}
   end else if Signature='MIDI' then begin
    if BlockSize=sizeof(TBeRoXMMIDIConfig) then begin
     if Read(MIDIConfig,sizeof(TBeRoXMMIDIConfig))<>sizeof(TBeRoXMMIDIConfig) then begin
      result:=false;
      exit;
     end;
     EmbeddedMIDIConfig:=true;
    end;
{$endif}
   end else if Signature='PNAM' then begin
    ItemSize:=BlockSize;
    if (ItemSize>=32) and (ItemSize<=(BeRoXMLastPattern*32)) then begin
     for Counter:=0 to (ItemSize div 32)-1 do begin
      if Read(C32,32)<>32 then begin
       result:=false;
       exit;
      end;
      if assigned(Pattern[Counter]) then begin
       Pattern[Counter].Name:=trim(C32);
      end;
     end;
    end;
   end else if Signature='CNAM' then begin
    ItemSize:=BlockSize;
    if (ItemSize>=20) and (ItemSize<=(NumberOfChannels*20)) then begin
     for Counter:=0 to (ItemSize div 20)-1 do begin
      FillChar(C32,32,#0);
      if Read(C32,20)<>20 then begin
       result:=false;
       exit;
      end;
      Channel[Counter].Name:=trim(C32);
     end;
    end;
   end else if Signature='CHFX' then begin
    for Counter:=0 to NumberOfChannels-1 do begin
     if (Counter*sizeof(longword))<integer(BlockSize) then begin
      if Read(Value,sizeof(longword))<>sizeof(longword) then begin
       result:=false;
       exit;
      end;
      SwapLittleEndianData32(Value);
      Channel[Counter].Plugin:=Value;
     end;
    end;
   end else if Signature='EQFX' then begin
    EQFXData.Clear;
    StreamRead(EQFXData,BlockSize);
    EQFXData.Seek(0);
   end else if (Signature[0]='F') and (Signature[1]='X') and (Signature[2] in ['0'..'9']) and (Signature[3] in ['0'..'9']) then begin
    Value:=((byte(Signature[2])-byte('0'))*10)+(byte(Signature[3])-byte('0'));
    if Value in [0..99] then begin
     FXData[Value].Clear;
     StreamRead(FXData[Value],BlockSize);
     FXData[Value].Seek(0);
    end;
   end else if Signature='MODU' then begin
    ModularData.Clear;
    StreamRead(ModularData,BlockSize);
    ModularData.Seek(0);
   end else begin
    break;
   end;
   DataPosition:=BlockLastPosition+BlockSize+8;
  end;
  DataPosition:=LastPosition;
  result:=true;
 end;
begin
 result:=false;
 Clear;
 DataPosition:=0;
 if Read(Header,sizeof(TXMHeader))<>sizeof(TXMHeader) then begin
  Clear;
  result:=false;
  exit;
 end;
 SwapLittleEndianData16(Header.Version);
 SwapLittleEndianData32(Header.Size);
 SwapLittleEndianData16(Header.TrackLength);
 SwapLittleEndianData16(Header.RestartPosition);
 SwapLittleEndianData16(Header.NumberOfChannels);
 SwapLittleEndianData16(Header.NumberOfPatterns);
 SwapLittleEndianData16(Header.NumberOfInstruments);
 SwapLittleEndianData16(Header.Flags);
 SwapLittleEndianData16(Header.Tempo);
 SwapLittleEndianData16(Header.Speed);
 if (Header.Signature<>'Extended Module: ') or (Header.NumberOfChannels>BeRoXMLastChannel) or (Header.NumberOfInstruments>BeRoXMLastInstrument) then begin
  Clear;
{$ifdef UseAmigaMODLoader}
  result:=LoadMOD;
{$else}
  result:=false;
{$endif}
  exit;
 end;

 Name:=trim(Header.Name);
 TrackLength:=Header.TrackLength;
 RestartPosition:=Header.RestartPosition;
 NumberOfChannels:=Header.NumberOfChannels;
 NumberOfPatterns:=Header.NumberOfPatterns;
 NumberOfInstruments:=Header.NumberOfInstruments;

 LinearSlides:=(Header.Flags and $1)<>0;
 ExtendedFilterRange:=(Header.Flags and $1000)<>0;

 StartSpeed:=Header.Speed;
 StartTempo:=Header.Tempo;
 if StartSpeed=0 then StartSpeed:=6;
 if StartTempo=0 then StartTempo:=125;
 Speed:=StartSpeed;
 Tempo:=StartTempo;

 PatternOrder:=Header.PatternOrder;

 inc(DataPosition,integer(Header.Size-(sizeof(TXMHeader)-60)));

 FillChar(EmptyNote,sizeof(TBeRoXMPatternNote),#0);

 for Counter:=0 to NumberOfChannels-1 do begin
  Channel[Counter].StandardPanning:=128;
 end;

 if Header.Version<=$103 then begin
  FillChar(InstrumentSamplesCount,sizeof(InstrumentSamplesCount),#0);
  if not LoadInstruments then exit;
  if not LoadPatterns then exit;
  if not LoadInstrumentSamples then exit;
 end else begin
  if not LoadPatterns then exit;
  if not LoadInstruments then exit;
 end;

 IsTrackActive:=true;

 LoadExtendedData;

 result:=true;
end;

function TBeRoXMModule.Load(Data:TBeRoXMStream):boolean;
var DataPointer:pointer;
begin
 GetMem(DataPointer,Data.Size);
 Data.Seek(0);
 if Data.Read(DataPointer^,Data.Size)=Data.Size then begin
  result:=Load(Data,Data.Size);
 end else begin
  result:=false;
 end;
 FreeMem(DataPointer);
end;

function TBeRoXMModule.LoadFile(FileName:string):boolean;
var TheFile:file;
    Data:pointer;
    Size:longword;
    OldFileMode:byte;
begin
 OldFileMode:=FileMode;
 result:=false;
 FileMode:=0;
 AssignFile(TheFile,FileName);
 {$I-}System.Reset(TheFile,1);{$I+}
 if IOResult<>0 then begin
  FileMode:=OldFileMode;
  exit;
 end;
 Size:=FileSize(TheFile);
 GetMem(Data,Size);
 BlockRead(TheFile,Data^,Size);
 CloseFile(TheFile);
 FileMode:=OldFileMode;
 result:=Load(Data,Size);
 FreeMem(Data);
end;

function TBeRoXMModule.LoadFromResource(Instance:THandle;const ResName:string;ResType:pchar):boolean;
{$ifdef win32}
var ResourceHandle,Handle:THandle;
    Data:pointer;
    Size:longword;
{$endif}
begin
 result:=false;
{$ifdef win32}
 ResourceHandle:=FindResource(Instance,pchar(ResName),ResType);
 if ResourceHandle<>0 then begin
  Handle:=LoadResource(Instance,ResourceHandle);
  if Handle<>0 then begin
   Data:=LockResource(Handle);
   if assigned(Data) then begin
    Size:=SizeOfResource(Instance,ResourceHandle);
    if Size>0 then begin
     result:=Load(Data,Size);
    end;
   end;
{$ifndef WINCE}
{$ifndef fpc}
   UnlockResource(Handle);
{$endif}
   FreeResource(Handle);
{$endif}
  end;
 end;
{$endif}
end;

{$ifdef UseSaver}
function TBeRoXMModule.Save(Data:TBeRoXMStream;SampleCompressionThreshold:integer=100):boolean;
{$ifdef UseDPCM4}
const DPCM4CompressionTables:array[0..2] of TBeRoXMDPCM4Table=((0,1,2,4,8,16,32,64,-1,-2,-4,-8,-16,-32,-48,-64), // u-Law table
                                                               (0,1,2,3,5,7,12,19,-1,-2,-3,-5,-7,-12,-19,-31), // Linear table
                                                               (0,1,2,3,6,10,15,21,-0,-1,-2,-3,-6,-10,-15,-21)); // Sierra new table
{$endif}
var Header:TXMHeader;
    InstrumentHeader:TXMInstrumentHeader;
    InstrumentExtraHeader:TXMInstrumentExtraHeader;
    SampleHeader:TXMSampleHeader;
    Count,Counter,SubCounter,SubSubCounter,SampleCounter,SourceValue:integer;
    Sample:TBeRoXMSample;
    SampleBuffer:pointer;
    LastByte,DeltaByte:shortint;
    LastWord,DeltaWord:smallint;
    DestByte:pshortint;
    DestWord:psmallint;
    NumberOfInstruments,NumberOfPatterns,NumberOfSamples:integer;
    TrackComment:string;
    PatternNamesSize,ChannelNamesSize:longword;
    Signature:TBeRoXMChunkSignature;
    Value:longword;
    ByteValue:byte;
    FXPlugins:boolean;
{$ifdef UseDPCM4}
    DPCM4CompressionTable:TBeRoXMDPCM4Table;
    SampleCompressionError:integer;
 function DPCM4PackSample(var Sample:integer;Next:integer):longword;
 var Delta:integer;
 begin
  Delta:=Next-Sample;
  if Delta>=0 then begin
   result:=0;
   while result<7 do begin
    if Delta<=DPCM4CompressionTable[result+1] then begin
     break;
    end;
    inc(result);
   end;
  end else begin
   result:=8;
   while result<15 do begin
    if Delta>=DPCM4CompressionTable[result+1] then begin
     break;
    end;
    inc(result);
   end;
  end;
  inc(Sample,DPCM4CompressionTable[result]);
 end;
 function DPCM4CanPackSample(SampleData:PBeRoXMSampleData;Len,ErrorThreshold:integer;var ResultError:integer):boolean;
 var i,j,Error,Total,Pos,OldPos,Old,s,BestError,BestTable:integer;
 begin
  result:=false;
  ResultError:=0;
  if Len<1024 then begin
   exit;
  end;
  BestError:=0;
  BestTable:=0;
  for j:=0 to length(DPCM4CompressionTables)-1 do begin
   DPCM4CompressionTable:=DPCM4CompressionTables[j];
   Error:=0;
   Total:=1;
   Pos:=0;
   OldPos:=0;
   Old:=0;
   for i:=0 to Len-1 do begin
    s:=SampleData^[i] div 256;
    DPCM4PackSample(Pos,s);
    inc(Error,abs(Pos-OldPos));
    inc(Total,abs(s-Old));
    Old:=s;
    OldPos:=Pos;
   end;
   Error:=(Error*int64(100)) div Total;
   if Error>=BestError then begin
    BestError:=Error;
    BestTable:=j;
   end;
  end;
  DPCM4CompressionTable:=DPCM4CompressionTables[BestTable];
  if BestError>100 then begin
   ResultError:=100;
  end else begin
   ResultError:=BestError;
  end;
  result:=BestError>=ErrorThreshold;
 end;
 function DPCM4CompressSample(SampleData:PBeRoXMSampleData;Len:integer):integer;
 var Pos,i:integer;
     b:byte;
     p:PBeRoXMByteArray;
 begin
  result:=((Len+1) div 2);
  Data.Write(DPCM4CompressionTable,sizeof(TBeRoXMDPCM4Table));
  GetMem(p,result);
  Pos:=0;
  for i:=0 to result-1 do begin
   b:=DPCM4PackSample(pos,SampleData^[i*2] div 256) and $f;
   b:=b or ((DPCM4PackSample(pos,SampleData^[(i*2)+1] div 256) and $f) shl 4);
   p^[i]:=b;
  end;
  Data.Write(p^,result*sizeof(byte));
  FreeMem(p);
  inc(result,16);
 end;
{$endif}
{function DWDPCM8CompressSamplePart(SampleData:PBeRoXMSampleData;Len,Granularity:integer;Output:PBeRoXMByteArray):integer;
 var UseSecondDifference:boolean;
     Position,Sample,Width,EscapeCode,Last,LastFirstDifference,FirstDifference,Code,BitBuffer,BitBufferFreeBits,Size:integer;
  procedure PutBits(Value,Bits:integer);
  var i:integer;
  begin
   while Bits>0 do begin
    i:=Bits;
    if i>BitBufferFreeBits then begin
     i:=BitBufferFreeBits;
    end;
    BitBuffer:=(BitBuffer shl i) or ((Value shr (Bits-i)) and ((1 shl i)-1));
    dec(BitBufferFreeBits,i);
    if BitBufferFreeBits=0 then begin
     Output^[Size]:=byte(BitBuffer and $ff);
     inc(Size);
     BitBufferFreeBits:=8;
    end;
    dec(Bits,i);
   end;
  end;
 begin
  Size:=0;
  BitBuffer:=0;
  BitBufferFreeBits:=8;
  UseSecondDifference:=Granularity<0;
  if UseSecondDifference then begin
   Granularity:=-Granularity;
  end;
  Width:=Granularity;
  EscapeCode:=-(1 shl (Width-1));
  Last:=0;
  LastFirstDifference:=0;
  for Position:=0 to Len-1 do begin
   Sample:=SampleData^[Position] div 256;
   FirstDifference:=Last-Sample;
   Last:=Sample;
   if UseSecondDifference then begin
    Code:=LastFirstDifference-FirstDifference;
    LastFirstDifference:=FirstDifference;
   end else begin
    Code:=FirstDifference;
   end;
   Code:=shortint(byte(Code+0)+0);
   while ((-abs(Code))<=EscapeCode) and (Width<8) do begin
    PutBits(EscapeCode,Width);
    inc(Width);
    EscapeCode:=-(1 shl (Width-1));
   end;
   PutBits(Code,Width);
   if (Width>Granularity) and ((-abs(Code))>(-(1 shl (Width-2)))) then begin
    dec(Width);
    EscapeCode:=-(1 shl (Width-1));
   end;
  end;
  if BitBufferFreeBits<>8 then begin
   PutBits(0,BitBufferFreeBits);
  end;
  result:=Size;
 end;
 function DWDPCM8CompressSample(SampleData:PBeRoXMSampleData;Len:integer):integer;
 var p:PBeRoXMByteArray;
     Granularity,BestGranularity,OldBestGranularity,Size,BestSize:integer;
 begin
  if Len>0 then begin
   GetMem(p,Len*4);
   BestSize:=Len*4;
   BestGranularity:=8;
   for Granularity:=2 to 8 do begin
    Size:=DWDPCM8CompressSamplePart(SampleData,Len,Granularity,p);
    if Size<=BestSize then begin
     BestSize:=Size;
     BestGranularity:=Granularity;
    end else begin
     break;
    end;
   end;
   OldBestGranularity:=BestGranularity;
   for Granularity:=-(OldBestGranularity-1) to -1 do begin
    Size:=DWDPCM8CompressSamplePart(SampleData,Len,Granularity,p);
    if Size<BestSize then begin
     BestSize:=Size;
     BestGranularity:=Granularity;
    end else begin
     break;
    end;
   end;
   result:=DWDPCM8CompressSamplePart(SampleData,Len,BestGranularity,p);
   Data.Write(p^,result);
   FreeMem(p);
  end else begin
   result:=0;
  end;
 end;}
 procedure WritePattern(PatternNummer:integer);
 var Row,Channel:integer;
     PatternHeader:TXMPatternHeader;
     PatternNote:TBeRoXMPatternNote;
     PatternDaten:TBeRoXMMemoryStream;
     Flags:byte;
  procedure WriteByte(B:byte);
  begin
   PatternDaten.Write(B,sizeof(byte));
  end;
 begin
  PatternDaten:=TBeRoXmMemoryStream.Create;
  FillChar(PatternHeader,sizeof(TXMPatternHeader),#0);
  PatternHeader.Size:=SwapDWordLittleEndian(9);
  PatternHeader.PackingType:=0;
  PatternHeader.Rows:=SwapWordLittleEndian(Pattern[PatternNummer].Rows);
  if not Pattern[PatternNummer].IsEmpty then begin
   for Row:=0 to Pattern[PatternNummer].Rows-1 do begin
    for Channel:=0 to Pattern[PatternNummer].Channels-1 do begin
     PatternNote:=Pattern[PatternNummer].GetNote(Row,Channel);
     if (PatternNote.Note<>0) and (PatternNote.Instrument<>0) and (PatternNote.Volume>$f) and ((PatternNote.Effect<>BeRoXMEffectArpeggio) or (PatternNote.EffectParameter<>0)) then begin
      WriteByte(PatternNote.Note);
      WriteByte(PatternNote.Instrument);
      WriteByte(PatternNote.Volume);
      WriteByte(PatternNote.Effect);
      WriteByte(PatternNote.EffectParameter);
     end else begin
      Flags:=$80;
      if PatternNote.Note<>0 then Flags:=Flags or 1;
      if PatternNote.Instrument<>0 then Flags:=Flags or 2;
      if PatternNote.Volume>$f then Flags:=Flags or 4;
      if PatternNote.Effect<>BeRoXMEffectArpeggio then Flags:=Flags or 8;
      if PatternNote.EffectParameter<>0 then Flags:=Flags or 16;
      WriteByte(Flags);
      if (Flags and 1)<>0 then WriteByte(PatternNote.Note);
      if (Flags and 2)<>0 then WriteByte(PatternNote.Instrument);
      if (Flags and 4)<>0 then WriteByte(PatternNote.Volume);
      if (Flags and 8)<>0 then WriteByte(PatternNote.Effect);
      if (Flags and 16)<>0 then WriteByte(PatternNote.EffectParameter);
     end;
    end;
   end;
  end;
  PatternHeader.PackedSize:=SwapWordLittleEndian(PatternDaten.Size);
  Data.Write(PatternHeader,sizeof(TXMPatternHeader));
  Data.Append(PatternDaten);
  PatternDaten.Free;
 end;
begin
 result:=false;
 Data.Clear;

 NumberOfPatterns:=0;
 for Counter:=0 to BeRoXMLastPattern do begin
  if Pattern[Counter].Rows>0 then begin
   NumberOfPatterns:=Counter+1;
  end;
 end;

 NumberOfInstruments:=0;
 for Counter:=1 to BeRoXMLastInstrument do begin
  if assigned(Instrument[Counter]) then begin
   NumberOfInstruments:=Counter;
  end;
 end;

 FillChar(Header,sizeof(TXMHeader),#0);
 Header.Signature:='Extended Module: ';

 for Counter:=1 to 20 do begin
  Header.Name[Counter-1]:=#0;
 end;
 Count:=length(Name);
 if Count>20 then begin
  Count:=20;
 end;
 Move(Name[1],Header.Name,Count);

 Header.End1AValue:=$1a;
 Header.Tracker:='FastTracker v2.00   ';
 Header.Version:=SwapWordLittleEndian($0104);
 Header.Size:=SwapDWordLittleEndian(sizeof(TXMHeader)-60);
 Header.TrackLength:=SwapWordLittleEndian(TrackLength);
 Header.RestartPosition:=SwapWordLittleEndian(RestartPosition);
 Header.NumberOfChannels:=SwapWordLittleEndian(NumberOfChannels);
 Header.NumberOfInstruments:=SwapWordLittleEndian(NumberOfInstruments);
 Header.NumberOfPatterns:=SwapWordLittleEndian(NumberOfPatterns);
 Header.PatternOrder:=PatternOrder;

 Header.Flags:=0;
 if LinearSlides then Header.Flags:=Header.Flags or $1;
 if ExtendedFilterRange then Header.Flags:=Header.Flags or $1000;
 SwapLittleEndianData16(Header.Flags);

 Header.Speed:=SwapWordLittleEndian(StartSpeed);
 Header.Tempo:=SwapWordLittleEndian(StartTempo);

 if Data.Write(Header,sizeof(TXMHeader))<>sizeof(TXMHeader) then exit;

 for Counter:=0 to NumberOfPatterns-1 do WritePattern(Counter);

 for Counter:=1 to NumberOfInstruments do begin
  FillChar(InstrumentHeader,sizeof(TXMInstrumentHeader),#0);
  FillChar(InstrumentExtraHeader,sizeof(TXMInstrumentExtraHeader),#0);
  NumberOfSamples:=0;
  if assigned(Instrument[Counter]) then begin
   for SubCounter:=0 to BeRoXMLastSample do begin
    if assigned(Instrument[Counter].Sample[SubCounter]) then begin
     NumberOfSamples:=SubCounter+1;
    end;
   end;
  end;
  if NumberOfSamples>0 then begin
   InstrumentHeader.Size:=sizeof(TXMInstrumentHeader)+sizeof(TXMInstrumentExtraHeader);
  end else begin
   InstrumentHeader.Size:=sizeof(TXMInstrumentHeader);
  end;
  for SubCounter:=1 to 21 do begin
   InstrumentHeader.Name[SubCounter]:=#0;
  end;
  if assigned(Instrument[Counter]) then begin
   Count:=length(Instrument[Counter].Name);
   if Count>21 then begin
    Count:=21;
   end;
   if Count>0 then begin
    MOVE(Instrument[Counter].Name[1],InstrumentHeader.Name,Count);
   end;
  end;
  InstrumentHeader.InstrumentType:=0;
  InstrumentHeader.CountOfSamples:=SwapWordLittleEndian(NumberOfSamples);
  if NumberOfSamples>0 then begin
   InstrumentExtraHeader.Size:=SwapDWordLittleEndian(sizeof(TXMSampleHeader));
   InstrumentExtraHeader.FadeOut:=SwapWordLittleEndian(Instrument[Counter].FadeOut);
   for SubCounter:=1 to 96 do begin
    InstrumentExtraHeader.SampleMap[SubCounter]:=Instrument[Counter].SampleMap[SubCounter];
   end;
   InstrumentExtraHeader.VolumeEnvelopeNumPoints:=Instrument[Counter].VolumeEnvelope.NumberOfPoints;
   InstrumentExtraHeader.PanningEnvelopeNumPoints:=Instrument[Counter].PanningEnvelope.NumberOfPoints;
   if InstrumentExtraHeader.VolumeEnvelopeNumPoints>12 then InstrumentExtraHeader.VolumeEnvelopeNumPoints:=12;
   if InstrumentExtraHeader.PanningEnvelopeNumPoints>12 then InstrumentExtraHeader.PanningEnvelopeNumPoints:=12;
   for SubCounter:=0 to 12-1 do begin
    InstrumentExtraHeader.VolumeEnvelope[SubCounter*2]:=SwapWordLittleEndian(Instrument[Counter].VolumeEnvelope.Points[SubCounter].Tick);
    InstrumentExtraHeader.VolumeEnvelope[(SubCounter*2)+1]:=SwapWordLittleEndian(Instrument[Counter].VolumeEnvelope.Points[SubCounter].Value);
    InstrumentExtraHeader.PanningEnvelope[SubCounter*2]:=SwapWordLittleEndian(Instrument[Counter].PanningEnvelope.Points[SubCounter].Tick);
    InstrumentExtraHeader.PanningEnvelope[(SubCounter*2)+1]:=SwapWordLittleEndian(Instrument[Counter].PanningEnvelope.Points[SubCounter].Value);
   end;
   InstrumentExtraHeader.VolumeEnvelopeSustain:=Instrument[Counter].VolumeEnvelope.SustainPoint;
   InstrumentExtraHeader.VolumeEnvelopeLoopStart:=Instrument[Counter].VolumeEnvelope.LoopStartPoint;
   InstrumentExtraHeader.VolumeEnvelopeLoopEnd:=Instrument[Counter].VolumeEnvelope.LoopEndPoint;
   if Instrument[Counter].VolumeEnvelope.Active then InstrumentExtraHeader.VolFlags:=InstrumentExtraHeader.VolFlags or 1;
   if Instrument[Counter].VolumeEnvelope.Sustain then InstrumentExtraHeader.VolFlags:=InstrumentExtraHeader.VolFlags or 2;
   if Instrument[Counter].VolumeEnvelope.Loop then InstrumentExtraHeader.VolFlags:=InstrumentExtraHeader.VolFlags or 4;

   InstrumentExtraHeader.PanningEnvelopeSustain:=Instrument[Counter].PanningEnvelope.SustainPoint;
   InstrumentExtraHeader.PanningEnvelopeLoopStart:=Instrument[Counter].PanningEnvelope.LoopStartPoint;
   InstrumentExtraHeader.PanningEnvelopeLoopEnd:=Instrument[Counter].PanningEnvelope.LoopEndPoint;
   if Instrument[Counter].PanningEnvelope.Active then InstrumentExtraHeader.PanFlags:=InstrumentExtraHeader.PanFlags or 1;
   if Instrument[Counter].PanningEnvelope.Sustain then InstrumentExtraHeader.PanFlags:=InstrumentExtraHeader.PanFlags or 2;
   if Instrument[Counter].PanningEnvelope.Loop then InstrumentExtraHeader.PanFlags:=InstrumentExtraHeader.PanFlags or 4;

   InstrumentExtraHeader.VibratoType:=Instrument[Counter].VibratoType;
   InstrumentExtraHeader.VibratoSweep:=Instrument[Counter].VibratoSweep;
   InstrumentExtraHeader.VibratoDepth:=Instrument[Counter].VibratoDepth;
   InstrumentExtraHeader.VibratoRate:=Instrument[Counter].VibratoRate;
  end;
  if Data.Write(InstrumentHeader,sizeof(TXMInstrumentHeader))<>sizeof(TXMInstrumentHeader) then exit;
  if NumberOfSamples>0 then begin
   if Data.Write(InstrumentExtraHeader,sizeof(TXMInstrumentExtraHeader))<>sizeof(TXMInstrumentExtraHeader) then exit;
   for SubCounter:=0 to NumberOfSamples-1 do begin
    FillChar(SampleHeader,sizeof(TXMSampleHeader),#0);
    if assigned(Instrument[Counter].Sample[SubCounter]) then begin
     Sample:=Instrument[Counter].Sample[SubCounter];

     for SubSubCounter:=1 to 22 do begin
      SampleHeader.Name[SubSubCounter]:=#0;
     end;
     Count:=length(Sample.Name);
     if Count>22 then begin
      Count:=22;
     end;
     Move(Sample.Name[1],SampleHeader.Name,Count);

     SampleHeader.SampleLength:=SwapDWordLittleEndian(Sample.SampleLength);
     SampleHeader.LoopStart:=SwapDWordLittleEndian(Sample.LoopStart);
     SampleHeader.LoopLength:=SwapDWordLittleEndian(Sample.LoopLength);
     SampleHeader.Volume:=Sample.Volume;
     SampleHeader.FineTune:=Sample.FineTune;
     SampleHeader.RelativeNote:=Sample.RelativeNote;
     SampleHeader.Flags:=0;
     if Sample.PingPongLoop then begin
      SampleHeader.Flags:=SampleHeader.Flags or 2;
     end else if Sample.Loop then begin
      SampleHeader.Flags:=SampleHeader.Flags or 1;
     end;
     if Sample.Bits=16 then begin
      SampleHeader.Flags:=SampleHeader.Flags or 16;
      SampleHeader.SampleLength:=SwapDWordLittleEndian(SwapDWordLittleEndian(SampleHeader.SampleLength)*2);
      SampleHeader.LoopStart:=SwapDWordLittleEndian(SwapDWordLittleEndian(SampleHeader.LoopStart)*2);
      SampleHeader.LoopLength:=SwapDWordLittleEndian(SwapDWordLittleEndian(SampleHeader.LoopLength)*2);
     end;
     if Sample.Channels=2 then begin
      SampleHeader.Flags:=SampleHeader.Flags or 32;
      SampleHeader.SampleLength:=SwapDWordLittleEndian(SwapDWordLittleEndian(SampleHeader.SampleLength)*2);
      SampleHeader.LoopStart:=SwapDWordLittleEndian(SwapDWordLittleEndian(SampleHeader.LoopStart)*2);
      SampleHeader.LoopLength:=SwapDWordLittleEndian(SwapDWordLittleEndian(SampleHeader.LoopLength)*2);
     end;
     SampleHeader.Panning:=Sample.Panning;
     SampleHeader.Reserved:=$00;
{$ifdef UseDPCM4}
     Sample.DPCM4:=false;
     if (SampleHeader.Reserved=$00) and (SampleCompressionThreshold<>100) and ((Sample.Bits=8) and (Sample.Channels=1)) then begin
      if DPCM4CanPackSample(Sample.Data,Sample.SampleLength,SampleCompressionThreshold,SampleCompressionError) then begin
       if SampleCompressionError<100 then begin
        SampleHeader.Reserved:=$ad;
        Sample.DPCM4:=true;
       end;
      end;
     end;
{$endif}
    end;
    if Data.Write(SampleHeader,sizeof(TXMSampleHeader))<>sizeof(TXMSampleHeader) then begin
     result:=false;
     exit;
    end;
   end;
   for SubCounter:=0 to NumberOfSamples-1 do begin
    if assigned(Instrument[Counter].Sample[SubCounter]) then begin
     Sample:=Instrument[Counter].Sample[SubCounter];
     if Sample.Channels=1 then begin
      if Sample.Bits=8 then begin
{$ifdef UseDPCM4}
       if Sample.DPCM4 then begin
        DPCM4CanPackSample(Sample.Data,Sample.SampleLength,SampleCompressionThreshold,SampleCompressionError);
        DPCM4CompressSample(Sample.Data,Sample.SampleLength);
       end else{$endif}begin
        GetMem(SampleBuffer,Sample.SampleLength);
        DestByte:=SampleBuffer;
        LastByte:=0;
        for SampleCounter:=0 to Sample.SampleLength-1 do begin
         SourceValue:=Sample.Data^[SampleCounter] div 256;
         if SourceValue<-128 then begin
          SourceValue:=-128;
         end else if SourceValue>127 then begin
          SourceValue:=127;
         end;
         DeltaByte:=SourceValue-LastByte;
         DestByte^:=DeltaByte;
         LastByte:=SourceValue;
         inc(DestByte);
        end;
        if Data.Write(SampleBuffer^,Sample.SampleLength)<>integer(Sample.SampleLength) then begin
         result:=false;
         exit;
        end;
        FreeMem(SampleBuffer);
       end;
      end else if Sample.Bits=16 then begin
       GetMem(SampleBuffer,Sample.SampleLength*2);
       DestWord:=SampleBuffer;
       LastWord:=0;
       for SampleCounter:=0 to Sample.SampleLength-1 do begin
        SourceValue:=Sample.Data^[SampleCounter];
        if SourceValue<-32768 then begin
         SourceValue:=-32768;
        end else if SourceValue>32767 then begin
         SourceValue:=32767;
        end;
        DeltaWord:=SourceValue-LastWord;
        DestWord^:=SwapWordLittleEndian(DeltaWord);
        LastWord:=Sourcevalue;
        inc(DestWord);
       end;
       if Data.Write(SampleBuffer^,Sample.SampleLength*2)<>integer(Sample.SampleLength*2) then begin
        result:=false;
        exit;
       end;
       FreeMem(SampleBuffer);
      end;
     end else if Sample.Channels=2 then begin
      if Sample.Bits=8 then begin
       GetMem(SampleBuffer,Sample.SampleLength);
       DestByte:=SampleBuffer;
       LastByte:=0;
       for SampleCounter:=0 to Sample.SampleLength-1 do begin
        SourceValue:=Sample.Data^[SampleCounter*2] div 256;
        if SourceValue<-128 then begin
         SourceValue:=-128;
        end else if SourceValue>127 then begin
         SourceValue:=127;
        end;
        DeltaByte:=SourceValue-LastByte;
        DestByte^:=DeltaByte;
        LastByte:=SourceValue;
        inc(DestByte);
       end;
       if Data.Write(SampleBuffer^,Sample.SampleLength)<>integer(Sample.SampleLength) then begin
        result:=false;
        exit;
       end;
       DestByte:=SampleBuffer;
       LastByte:=0;
       for SampleCounter:=0 to Sample.SampleLength-1 do begin
        SourceValue:=Sample.Data^[(SampleCounter*2)+1] div 256;
        if SourceValue<-128 then begin
         SourceValue:=-128;
        end else if SourceValue>127 then begin
         SourceValue:=127;
        end;
        DeltaByte:=SourceValue-LastByte;
        DestByte^:=DeltaByte;
        LastByte:=SourceValue;
        inc(DestByte);
       end;
       if Data.Write(SampleBuffer^,Sample.SampleLength)<>integer(Sample.SampleLength) then begin
        result:=false;
        exit;
       end;
       FreeMem(SampleBuffer);
      end else if Sample.Bits=16 then begin
       GetMem(SampleBuffer,Sample.SampleLength*2);
       DestWord:=SampleBuffer;
       LastWord:=0;
       for SampleCounter:=0 to Sample.SampleLength-1 do begin
        SourceValue:=Sample.Data^[SampleCounter*2];
        if SourceValue<-32768 then begin
         SourceValue:=-32768;
        end else if SourceValue>32767 then begin
         SourceValue:=32767;
        end;
        DeltaWord:=SourceValue-LastWord;
        DestWord^:=SwapWordLittleEndian(DeltaWord);
        LastWord:=Sourcevalue;
        inc(DestWord);
       end;
       if Data.Write(SampleBuffer^,Sample.SampleLength*2)<>integer(Sample.SampleLength*2) then begin
        result:=false;
        exit;
       end;
       DestWord:=SampleBuffer;
       LastWord:=0;
       for SampleCounter:=0 to Sample.SampleLength-1 do begin
        SourceValue:=Sample.Data^[(SampleCounter*2)+1];
        if SourceValue<-32768 then begin
         SourceValue:=-32768;
        end else if SourceValue>32767 then begin
         SourceValue:=32767;
        end;
        DeltaWord:=SourceValue-LastWord;
        DestWord^:=SwapWordLittleEndian(DeltaWord);
        LastWord:=Sourcevalue;
        inc(DestWord);
       end;
       if Data.Write(SampleBuffer^,Sample.SampleLength*2)<>integer(Sample.SampleLength*2) then begin
        result:=false;
        exit;
       end;
       FreeMem(SampleBuffer);
      end;
     end;
    end;
   end;
  end;
 end;
 if length(Comment)>0 then begin
  TrackComment:='';
  for Counter:=1 to length(Comment) do begin
   if Comment[Counter]<>#10 then begin
    TrackComment:=TrackComment+Comment[Counter];
   end;
  end;
  Value:=length(TrackComment);
  Data.Write('text',sizeof(TBeRoXMChunkSignature));
  SwapLittleEndianData32(Value);
  Data.Write(Value,sizeof(longword));
  SwapLittleEndianData32(Value);
  Data.Write(TrackComment[1],Value);
 end;
{$ifdef UseMIDIMacros}
 if EmbeddedMIDIConfig then begin
  Data.Write('MIDI',sizeof(TBeRoXMChunkSignature));
  Value:=sizeof(TBeRoXMMIDIConfig);
  SwapLittleEndianData32(Value);
  Data.Write(Value,sizeof(longword));
  if Data.Write(MIDIConfig,sizeof(TBeRoXMMIDIConfig))<>sizeof(TBeRoXMMIDIConfig) then begin
   result:=false;
   exit;
  end;
 end;
{$endif}
 if NumberOfPatterns>0 then begin
  PatternNamesSize:=NumberOfPatterns*32;
  while true do begin
   if (PatternNamesSize div 32)<1 then begin
    break;
   end else if length(Pattern[(PatternNamesSize div 32)-1].Name)=0 then begin
    dec(PatternNamesSize,32);
   end else begin
    break;
   end;
  end;
 end;
 if NumberOfChannels>0 then begin
  ChannelNamesSize:=NumberOfChannels*20;
  while true do begin
   if (ChannelNamesSize div 20)<1 then begin
    break;
   end else if length(Channel[(ChannelNamesSize div 20)-1].Name)=0 then begin
    dec(ChannelNamesSize,20);
   end else begin
    break;
   end;
  end;
 end;
 if PatternNamesSize>0 then begin
  Data.Write('PNAM',sizeof(TBeRoXMChunkSignature));
  SwapLittleEndianData32(PatternNamesSize);
  if Data.Write(PatternNamesSize,sizeof(longword))<>sizeof(longword) then begin
   result:=false;
   exit;
  end;
  SwapLittleEndianData32(PatternNamesSize);
  for Counter:=0 to (PatternNamesSize div 32)-1 do begin
   for SubCounter:=0 to 32-1 do begin
    if SubCounter<length(Pattern[Counter].Name) then begin
     ByteValue:=byte(Pattern[Counter].Name[SubCounter+1]);
    end else begin
     ByteValue:=0;
    end;
    if Data.Write(ByteValue,sizeof(byte))<>sizeof(byte) then begin
     result:=false;
     exit;
    end;
   end;
  end;
 end;
 if ChannelNamesSize>0 then begin
  Data.Write('CNAM',sizeof(TBeRoXMChunkSignature));
  SwapLittleEndianData32(ChannelNamesSize);
  if Data.Write(ChannelNamesSize,sizeof(longword))<>sizeof(longword) then begin
   result:=false;
   exit;
  end;
  SwapLittleEndianData32(ChannelNamesSize);
  for Counter:=0 to (ChannelNamesSize div 20)-1 do begin
   for SubCounter:=0 to 20-1 do begin
    if SubCounter<length(Channel[Counter].Name) then begin
     ByteValue:=byte(Channel[Counter].Name[SubCounter+1]);
    end else begin
     ByteValue:=0;
    end;
    if Data.Write(ByteValue,sizeof(byte))<>sizeof(byte) then begin
     result:=false;
     exit;
    end;
   end;
  end;
 end;
 FXPlugins:=false;
 for Counter:=0 to 99 do begin
  if FXData[Counter].Size>0 then begin
   Signature:='FX00';
   Signature[2]:=char(byte(byte('0')+(Counter div 10)));
   Signature[3]:=char(byte(byte('0')+(Counter mod 10)));
   Value:=FXData[Counter].Size;
   Data.Write(Signature,sizeof(TBeRoXMChunkSignature));
   SwapLittleEndianData32(Value);
   Data.Write(Value,sizeof(longword));
   Data.Append(FXData[Counter]);
   FXPlugins:=true;
  end;
 end;
 if EQFXData.Size>0 then begin
  Signature:='EQFX';
  Value:=EQFXData.Size;
  Data.Write(Signature,sizeof(TBeRoXMChunkSignature));
  SwapLittleEndianData32(Value);
  Data.Write(Value,sizeof(longword));
  Data.Append(EQFXData);
 end;
 if FXPlugins then begin
  Signature:='CHFX';
  Value:=NumberOfChannels*sizeof(longword);
  Data.Write(Signature,sizeof(TBeRoXMChunkSignature));
  SwapLittleEndianData32(Value);
  Data.Write(Value,sizeof(longword));
  for Counter:=0 to NumberOfChannels-1 do begin
   Value:=Channel[Counter].Plugin;
   SwapLittleEndianData32(Value);
   Data.Write(Value,sizeof(longword));
  end;
 end;
 if ModularData.Size>0 then begin
  Signature:='MODU';
  Value:=ModularData.Size;
  Data.Write(Signature,sizeof(TBeRoXMChunkSignature));
  SwapLittleEndianData32(Value);
  Data.Write(Value,sizeof(longword));
  Data.Append(ModularData);
 end;
 result:=true;
end;

function TBeRoXMModule.SaveFile(FileName:string;SampleCompressionThreshold:integer=100):boolean;
var TheFile:TBeRoXMFileStream;
begin
 TheFile:=TBeRoXMFileStream.CreateNew(FileName);
 result:=Save(TheFile,SampleCompressionThreshold);
 TheFile.Destroy;
end;
{$endif}

{$ifdef MixerCubicSpline}
constructor TBeRoXMCubicSpline.Create;
var Counter,TableLengthSize:integer;
    X,TableLength:{$ifdef cpuarm}single{$else}double{$endif};
begin
 inherited Create;
 TableLengthSize:=1 shl BeRoXMCubicSplineFracBits;
 TableLength:=1/TableLengthSize;
 if TableLengthSize>0 then begin
  for Counter:=0 to TableLengthSize-1 do begin
   X:=Counter*TableLength;
   Table[Counter,0]:=round((-0.5*X*X*X+1.0*X*X-0.5*X)*BeRoXMCubicSplineValueLength);
   Table[Counter,1]:=round((1.5*X*X*X-2.5*X*X+1.0)*BeRoXMCubicSplineValueLength);
   Table[Counter,2]:=round((-1.5*X*X*X+2.0*X*X+0.5*X)*BeRoXMCubicSplineValueLength);
   Table[Counter,3]:=round((0.5*X*X*X-0.5*X*X)*BeRoXMCubicSplineValueLength);
  end;
 end;
end;

destructor TBeRoXMCubicSpline.Destroy;
begin
 inherited Destroy;
end;
{$endif}

{$ifdef MixerWindowedFIR}
constructor TBeRoXMWindowedFIR.Create;
const Points=BeRoXMWindowedFIRWidth;
      ThePoints=Points;
      HalfPoints=Points*0.5;
      ExtendedPI=3.1415926535897932384626433832795;
var FracValue,Value,SincValue,WindowValue,WindowFactor,WindowParameter,
    OtherPosition,Position:{$ifdef cpuarm}single{$else}double{$endif};
    Counter,SubCounter:integer;
begin
 for Counter:=0 to BeRoXMWindowedFIRLength-1 do begin
  FracValue:=(Counter/BeRoXMWindowedFIRLength)-0.5;
  WindowFactor:=(2*ExtendedPI)/ThePoints;
  for SubCounter:=0 to Points-1 do begin
   OtherPosition:=SubCounter-FracValue;
   Position:=OtherPosition-HalfPoints;
   if abs(Position)<BeRoXMEpsilon then begin
    Value:=BeRoXMWindowedFIRCutOff;
   end else begin
    SincValue:=sin(BeRoXMWindowedFIRCutOff*Position*ExtendedPI)/(Position*ExtendedPI);
    WindowParameter:=OtherPosition*WindowFactor;
    WindowValue:=0.42-0.50*cos(WindowParameter)+0.08*cos(2.0*WindowParameter); // Blackman exact
    Value:=SincValue*WindowValue;
   end;
   Table[Counter,SubCounter]:=round(Value*BeRoXMWindowedFIRValueLength);
  end;
 end;
end;

destructor TBeRoXMWindowedFIR.Destroy;
begin
 inherited Destroy;
end;
{$endif}

constructor TBeRoXM.Create(TheOutputSampleRate:longword;TheBufferSize,TheBufferCount:integer;Stereo:boolean;TheSleepTime:integer{$ifdef UseDirectSound};WindowHandle:THANDLE{$endif};TheBits:integer=16;ThePrebufferCount:integer=0);
begin
 inherited Create;

 SoundOutputMode:=somNone;

 SleepTime:=TheSleepTime;

{$ifdef windows}
{$ifdef UseDirectSound}
 OwnerWindowHandle:=WindowHandle;
{$endif}

 WaveHandler:=nil;
{$endif}

 if TheOutputSampleRate=0 then TheOutputSampleRate:=44100;
 BufferSize:=TheBufferSize;
 BufferCount:=TheBufferCount;
 PrebufferCount:=ThePrebufferCount;

 OutputSamples:=0;

 Module:=TBeRoXMModule.Create(self);
 Module.SampleRate:=TheOutputSampleRate;
 Module.RampSamples:=(Module.SampleRate*BeRoXMRampSamples) div 44100;
 Module.FastRampSamples:=(Module.SampleRate*BeRoXMFastRampSamples) div 44100;
 Module.BufferSize:=TheBufferSize;

 ClickRemovalFactor:=(BeRoXMClickRemovalFactorLength*TheOutputSampleRate) div 44100;
 if ClickRemovalFactor<2 then ClickRemovalFactor:=2;

 ResamplingMethod:=BeRoXMMixerWindowedFIR;
 Clipping:=true;
 Oscillators:=false;
 Playing:=false;
 Looping:=true;

{$ifdef MixerCubicSpline}
 CubicSpline:=TBeRoXMCubicSpline.Create;
{$endif}
{$ifdef MixerWindowedFIR}
 WindowedFIR:=TBeRoXMWindowedFIR.Create;
{$endif}

 ClearData;

 Start:=false;

 OutputSampleRate:=TheOutputSampleRate;

{$ifdef UseFilters}
{$ifdef UseFilterCoefTables}
 FilterCoefCalculateTable(OutputSampleRate);
{$endif}
{$endif}

 case TheBits of
  8,16:begin
   OutputBits:=TheBits;
  end;
  else begin
   OutputBits:=16;
  end;
 end;

 if Stereo then begin
  OutputChannels:=2;
 end else begin
  OutputChannels:=1;
 end;
 BufferMul:=(OutputBits*OutputChannels) div 8;
 BufferSamples:=BufferSize;
 WorkBufferSize:=BufferSize*sizeof(longint)*OutputChannels;
 OutputBufferSize:=(BufferSize*OutputBits*OutputChannels) div 8;
 TotalBufferSize:=OutputBufferSize*BufferCount;

 GetMem(MixBufferData,WorkBufferSize);

{$ifdef windows}
 WaveFormat.wFormatTag:=WAVE_FORMAT_PCM;
 WaveFormat.wBitsPerSample:=OutputBits;
 WaveFormat.nChannels:=OutputChannels;
 WaveFormat.nBlockAlign:=(WaveFormat.nChannels*WaveFormat.wBitsPerSample) div 8;
 WaveFormat.nSamplesPerSec:=OutputSampleRate;
 WaveFormat.nAvgBytesPerSec:=WaveFormat.nSamplesPerSec*WaveFormat.nBlockAlign;
 WaveFormat.cbSize:=0;
{$endif}

 BuffersPrebuffered:=0;

 FillChar(RingBuffer,sizeof(TBeRoXMRingBuffer),#0);
 DSPRingBuffer:=nil;
 DSPRingBufferMaxPosition:=OutputBufferSize;
 DSPRingBufferPosition:=DSPRingBufferMaxPosition;

{$ifdef BeRoXMRingBuffer}
 if InitRingBuffer then begin
  SoundOutputMode:=somRingBuffer;
{$ifndef android}
  Threaded:=false;
{$endif}
 end else
{$endif}
{$ifdef sdl}
 if InitSDL then begin
  SoundOutputMode:=somSDL;
{$ifndef android}
{$ifdef sdlex}
  Threaded:=true;
{$else}
  Threaded:=false;
{$endif}
{$endif}
 end else
{$endif}
{$ifdef windows}
{$ifdef UseDirectSound}
 if InitDirectSound then begin
  SoundOutputMode:=somDirectSound;
  Threaded:=true;
 end else{$endif} if InitWaveOut then begin
  SoundOutputMode:=somWaveOut;
  Threaded:=true;
 end;
{$else}
{$ifdef unix}
{$ifndef android}
 if InitDevDSP then begin
  SoundOutputMode:=somDevDSP;
  Threaded:=true;
 end;
{$endif}
{$else}
 begin
  Threaded:=false;
 end;
{$endif}
{$endif}

{$ifndef android}
{$ifdef fpc}
 InitCriticalSection(ThreadCriticalSection);
{$else}
 InitializeCriticalSection(ThreadCriticalSection);
{$endif}

 if Threaded then begin
  ThreadTerminated:=false;
  ThreadSuspended:=true;
  ThreadIsSuspended:=false;
  ThreadResumeSemaphore:=SemaphoreInit;
  ThreadSuspendSemaphore:=SemaphoreInit;
  ThreadActiveSemaphore:=SemaphoreInit;
{$ifdef windows}
  ThreadHandle:=BeginThread(nil,0,@BeRoXMThreadProc,self,0,ThreadID);
{$else}
  ThreadHandle:=BeginThread(@BeRoXMThreadProc,pointer(self),ptruint(ThreadID));
{$endif}
{$ifdef windows}
{$ifdef fpc}
  ThreadSetPriority(ThreadHandle,THREAD_PRIORITY_TIME_CRITICAL);
{$else}
  SetThreadPriority(ThreadHandle,THREAD_PRIORITY_TIME_CRITICAL);
{$endif}
{$else}
  System.ThreadSetPriority(ThreadHandle,19);
{$endif}
 end else begin
  ThreadID:=0;
  ThreadHandle:=0;
 end;
{$endif}
end;

destructor TBeRoXM.Destroy;
{$ifdef windows}
var Counter:integer;
{$endif}
begin
{$ifndef android}
 if Threaded then begin
  Enter;
  ThreadTerminated:=true;
  Leave;
  if ThreadIsSuspended then begin
   if ThreadSuspended then begin
    ThreadSuspended:=false;
    SemaphorePost(ThreadResumeSemaphore);
    SemaphoreWait(ThreadActiveSemaphore);
   end else begin
    SemaphorePost(ThreadResumeSemaphore);
    while ThreadIsSuspended do begin
     sleep(10);
    end;
   end;
  end;
{$ifdef fpc}
  WaitForThreadTerminate(ThreadHandle,250);
  KillThread(ThreadHandle);
  WaitForThreadTerminate(ThreadHandle,5000);
{$else}
  WaitForSingleObject(ThreadHandle,250);
  TerminateThread(ThreadHandle,0);
  WaitForSingleObject(ThreadHandle,5000);
  if ThreadHandle<>0 then CloseHandle(ThreadHandle);
 {$endif}
  SemaphoreDestroy(ThreadResumeSemaphore);
  SemaphoreDestroy(ThreadSuspendSemaphore);
  SemaphoreDestroy(ThreadActiveSemaphore);
 end;
{$ifdef fpc}
 DoneCriticalSection(ThreadCriticalSection);
{$else}
 DeleteCriticalSection(ThreadCriticalSection);
{$endif}
{$endif}
 ClearData;
 case SoundOutputMode of
  somNONE:begin
  end;
{$ifdef windows}
  somWaveOut:begin
   waveOutReset(WaveOutHandle);
   for Counter:=0 to BufferCount-1 do begin
    while waveOutUnprepareHeader(WaveOutHandle,WaveHandler[Counter],sizeof(TWAVEHDR))=WAVERR_STILLPLAYING do begin
     sleep(25);
    end;
   end;
   waveOutReset(WaveOutHandle);
   waveOutClose(WaveOutHandle);
   for Counter:=0 to BufferCount-1 do begin
    FreeMem(WaveHandler[Counter].lpData);
    FreeMem(WaveHandler[Counter]);
    WaveHandler[Counter]:=nil;
   end;
   setlength(WaveHandler,0);
  end;
{$ifdef UseDirectSound}
  somDirectSound:begin
   DirectSoundBuffer.Stop;
   DirectSoundBuffer:=nil;
   DirectSoundBufferPrimary:=nil;
   DirectSound:=nil;
  end;
{$endif}
{$endif}
{$ifdef unix}
{$ifndef android}
  somDevDSP:begin
   __close(DevDSPFileHandle);
   FreeMem(DevDSPBuffer);
  end;
{$endif}
{$endif}
{$ifdef sdl}
  somSDL:begin
{$ifdef sdlex}
   DestroyRingBuffer(RingBuffer);
{$endif}
   FreeMem(DSPRingBuffer);
   SDL_CloseAudio;
  end;
{$endif}
{$ifdef BeRoXMRingBuffer}
  somRingBuffer:begin
   DestroyRingBuffer(RingBuffer);
   FreeMem(DSPRingBuffer);
  end;
{$endif}
 end;
 FreeMem(MixBufferData);
 Module.Destroy;
{$ifdef MixerWindowedFIR}
 WindowedFIR.Destroy;
{$endif}
{$ifdef MixerCubicSpline}
 CubicSpline.Destroy;
{$endif}
 inherited Destroy;
end;

{$ifdef windows}
{$ifdef UseDirectSound}
function TBeRoXM.InitDirectSound:boolean;
var Caps:TDSCAPS;
    P1,P2:pointer;
    S1,S2:longword;
begin
 result:=false;
 if DirectSoundCreate(nil,DirectSound,nil)<>DS_OK then exit;

 FillChar(Caps,sizeof(TDSCAPS),#0);
 Caps.Size:=sizeof(TDSCAPS);
 if DirectSound.GetCaps(Caps)<>DS_OK then exit;

 if DirectSound.SetCooperativeLevel(OwnerWindowHandle,DSSCL_PRIORITY)<>DS_OK then exit;

 FillChar(DSBufferDesc,sizeof(TDSBufferDesc),#0);
 DSBufferDesc.Size:=sizeof(TDSBufferDesc);
 DSBufferDesc.Reserved:=0;
 DSBufferDesc.Flags:=DSBCAPS_PRIMARYBUFFER;
 DSBufferDesc.BufferBytes:=0;
 DSBufferDesc.WaveFormatEx:=nil;

 if DirectSound.CreateSoundBuffer(DSBufferDesc,DirectSoundBufferPrimary,nil)<>DS_OK then exit;

 if DirectSoundBufferPrimary.SetFormat(WaveFormat)<>DS_OK then exit;

 FillChar(DSBufferDesc,sizeof(TDSBufferDesc),#0);
 DSBufferDesc.Size:=sizeof(TDSBufferDesc);
 DSBufferDesc.Reserved:=0;
 DSBufferDesc.Flags:=DSBCAPS_GLOBALFOCUS or DSBCAPS_CTRLFREQUENCY or DSBCAPS_GETCURRENTPOSITION2 or DSBCAPS_CTRLVOLUME;
 DSBufferDesc.BufferBytes:=TotalBufferSize;
 DSBufferDesc.WaveFormatEx:=@WaveFormat;

 if DirectSound.CreateSoundBuffer(DSBufferDesc,DirectSoundBuffer,nil)<>DS_OK then exit;

 DirectSoundBuffer.Lock(0,TotalBufferSize,P1,S1,P2,S2,DSBLOCK_ENTIREBUFFER);
 if S1<>0 then FillChar(P1^,S1,#0);
 if S2<>0 then FillChar(P2^,S2,#0);
 DirectSoundBuffer.Unlock(P1,S1,P2,S2);

 DirectSoundBasePosition:=0;
 
 DirectSoundBuffer.Play(0,0,DSBPLAY_LOOPING);
 result:=true;
end;
{$endif}

function TBeRoXM.InitWaveOut:boolean;
var Counter:integer;
    WOC:TWAVEOUTCAPSW;
begin
 setlength(WaveHandler,BufferCount);
 for Counter:=0 to BufferCount-1 do begin
  GetMem(WaveHandler[Counter],sizeof(TWAVEHDR));
  WaveHandler[Counter].dwFlags:=WHDR_DONE;
  GetMem(WaveHandler[Counter].lpData,OutputBufferSize);
  FillChar(WaveHandler[Counter].lpData^,OutputBufferSize,#0);
  WaveHandler[Counter].dwBufferLength:=OutputBufferSize;
  WaveHandler[Counter].dwBytesRecorded:=0;
  WaveHandler[Counter].dwUser:=0;
  WaveHandler[Counter].dwLoops:=0;
 end;
 result:=waveOutOpen(@WaveOutHandle,WAVE_MAPPER,@WaveFormat,0,0,0)=MMSYSERR_NOERROR;
 WaveOutIsSampleAccurate:=false;
 WaveOutLastGetPositionSample:=0;
 WaveOutAddGetPositionSample:=0;
 WaveOutFirstBufferTime:=-1;
 if result then begin
  if waveOutGetDevCapsW(0,@WOC,sizeof(TWAVEOUTCAPS))=0 then begin
   WaveOutIsSampleAccurate:=(WOC.dwSupport and WAVECAPS_SAMPLEACCURATE)<>0;
  end;
 end;
end;
{$endif}

{$ifdef unix}
{$ifndef android}
function TBeRoXM.InitDevDSP:boolean;
var i:integer;
begin
 try
  GetMem(DevDSPBuffer,OutputBufferSize);
  FillChar(DevDSPBuffer^,OutputBufferSize,#0);
  DevDSPFileHandle:=open('/dev/dsp',O_WRONLY);
  case OutputBits of
   8:i:=AFMT_U8;
   16:{$ifdef big_endian}i:=AFMT_S16_BE;{$else}i:=AFMT_S16_LE;{$endif}
   24:{$ifdef big_endian}i:=AFMT_S24_BE;{$else}i:=AFMT_S24_LE;{$endif}
   32:{$ifdef big_endian}i:=AFMT_S32_BE;{$else}i:=AFMT_S32_LE;{$endif}
   else i:=AFMT_U8;
  end;
  ioctl(DevDSPFileHandle,SNDCTL_DSP_SETFMT,@i);
  i:=OutputChannels;
  ioctl(DevDSPFileHandle,SNDCTL_DSP_CHANNELS,@i);
  i:=OutputSampleRate;
  ioctl(DevDSPFileHandle,SNDCTL_DSP_SPEED,@i);
  result:=true;
 except
  result:=false;
 end;
end;
{$endif}
{$endif}

{$ifdef sdl}
procedure SDLFillBuffer(UserData:pointer;Stream:puint8;Len:integer); cdecl;
var XM:TBeRoXM;
{$ifdef sdlex}
   EndTime:int64;
{$endif}
begin
 try
  XM:=UserData;
  if (Len>0) and (Len<=integer(XM.SDLWaveFormat.Size)) then begin
{$ifdef sdlex}
   EndTime:=SDL_GetTicks+1000;
   while (RingBufferUsed(XM.RingBuffer)<longword(Len)) and (SDL_GetTicks<EndTime) do begin
{$ifdef WIN32}
    sleep(1);
{$else}
    SDL_Delay(1);
{$endif}
   end;
   ReadFromRingBuffer(XM.RingBuffer,pointer(Stream),Len);
{$else}
   XM.GetBuffer(Stream,Len);
{$endif}
  end;
 except
 end;
end;

function TBeRoXM.InitSDL:boolean;
begin
 try
  if SDL_WasInit(SDL_INIT_AUDIO)=0 then begin
   if SDL_Init(SDL_INIT_AUDIO)<0 then begin
{$ifdef sdlconsole}
    writeln(SDL_GetError);
{$endif}
    result:=false;
    exit;
   end;
  end;
  FillChar(SDLWaveFormat,SIZEOF(TSDL_AudioSpec),#0);
  SDLWaveFormat.Channels:=OutputChannels;
  case OutputBits of
   8:SDLWaveFormat.Format:=AUDIO_U8;
   16:SDLWaveFormat.Format:=AUDIO_S16;
   else begin
    result:=false;
    exit;
   end;
  end;
  SDLWaveFormat.Freq:=Module.SampleRate;
  SDLWaveFormat.Callback:=@SDLFillBuffer;
  SDLWaveFormat.silence:=0;
  SDLWaveFormat.Samples:=BufferSamples;
  SDLWaveFormat.Size:=OutputBufferSize;
  SDLWaveFormat.UserData:=pointer(self);

{$ifdef sdlex}
  CreateRingBuffer(RingBuffer,OutputBufferSize*BufferCount);
{$endif}
  GetMem(DSPRingBuffer,OutputBufferSize);
  FillChar(DSPRingBuffer^,OutputBufferSize,#0);

  result:=SDL_OpenAudio(@SDLWaveFormat,nil)>=0;
  if result then begin
   DSPRingBufferMaxPosition:=SDLWaveFormat.Size;
   DSPRingBufferPosition:=DSPRingBufferMaxPosition;
   SDL_PauseAudio(1);
  end else begin
{$ifdef sdlconsole}
   writeln(SDL_GetError);
{$endif}
  end;
 except
  result:=false;
 end;
end;
{$endif}

{$ifdef BeRoXMRingBuffer}
function TBeRoXM.InitRingBuffer:boolean;
begin
 try
  CreateRingBuffer(RingBuffer,OutputBufferSize*BufferCount);
  GetMem(DSPRingBuffer,OutputBufferSize);
  FillChar(DSPRingBuffer^,OutputBufferSize,#0);
  result:=true;
 except
  result:=false;
 end;
end;
{$endif}

procedure TBeRoXM.Enter;
begin
 EnterCriticalSection(ThreadCriticalSection);
end;

procedure TBeRoXM.Leave;
begin
 LeaveCriticalSection(ThreadCriticalSection);
end;

procedure TBeRoXM.ClearData;
begin
 Module.Clear;
end;

procedure TBeRoXM.Clear;
begin
 Enter;
 ClearData;
 Leave;
end;

{$ifdef UseFilters}
{$ifdef UseFilterCoefTables}
procedure TBeRoXM.FilterCoefCalculateTable(SampleRate:integer);
 function CutOffToFrequency(CutOffValue,FilterModifierValue:integer):integer;
 var FC:single;
     TheFrequency:integer;
 begin
  if CutOffValue<0 then CutOffValue:=0;
  if CutOffValue>$7f then CutOffValue:=$7f;
  if Module.ExtendedFilterRange then begin
   FC:=110*pow(2,0.25+((CutOffValue*(FilterModifierValue+256)))/(20*512));
  end else begin
   FC:=110*pow(2,0.25+((CutOffValue*(FilterModifierValue+256)))/(24*512));
  end;
  TheFrequency:=round(FC);
  if TheFrequency<120 then begin
   result:=120;
  end else if TheFrequency>20000 then begin
   result:=20000;
  end else if (TheFrequency*2)>Module.SampleRate then begin
   result:=Module.SampleRate div 2;
  end else begin
   result:=TheFrequency;
  end;
 end;
var FC,FS,FG,FB0,FB1,DMPFAC,D,E,DIW:{$ifdef cpuarm}single{$else}double{$endif};
    CutOff,Resonance:integer;
begin
 for CutOff:=0 to $7f do begin
  for Resonance:=0 to $7f do begin
   FC:=CutOffToFrequency(CutOff,256);
   FS:=SampleRate;
   FC:=FC*((2*3.14159265358)/FS);
   DMPFAC:=2*pow(10,-((24/128)*Resonance)/20);
   D:=(1-DMPFAC)*FC;
   if D>2 then D:=2;
   D:=(DMPFAC-D)/FC;
   E:=1/(FC*FC);
   DIW:=(1+D+E);
   FG:=1/DIW;
   FB0:=(D+(E*2))/DIW;
   FB1:=(-E)/DIW;
   FilterCoefs[Resonance,CutOff].A0:=round(FG*BeRoXMFilterLength);
   FilterCoefs[Resonance,CutOff].B0:=round(FB0*BeRoXMFilterLength);
   FilterCoefs[Resonance,CutOff].B1:=round(FB1*BeRoXMFilterLength);
  end;
 end;
end;
{$endif}
{$endif}

procedure TBeRoXM.SetTickVariables;
begin
 if Module.Tempo=0 then Module.Tempo:=125;
 TickSamples:=(OutputSampleRate*5*128) div (Module.Tempo shl 8);
 TickSamplesCounter:=TickSamples;
 Module.TickSamples:=TickSamples;
end;

procedure TBeRoXM.ProcessTick;
begin
 Module.ProcessTick;
 SetTickVariables;
end;

procedure TBeRoXM.ResetSound;
{$ifdef UseDirectSound}
var P1,P2:pointer;
    S1,S2:longword;
{$endif}
begin
 case SoundOutputMode of
  somNone:begin
  end;
{$ifdef windows}
  somWaveOut:begin
   WaveOutFirstBufferTime:=-1;
   waveOutReset(WaveOutHandle);
  end;
{$ifdef UseDirectSound}
  somDirectSound:begin
   DirectSoundBuffer.Lock(0,TotalBufferSize,P1,S1,P2,S2,DSBLOCK_ENTIREBUFFER);
   if S1<>0 then FillChar(P1^,S1,#0);
   if S2<>0 then FillChar(P2^,S2,#0);
   DirectSoundBasePosition:=0;
   DirectSoundBuffer.Unlock(P1,S1,P2,S2);
  end;
{$endif}
{$endif}
 end;
 DSPRingBufferPosition:=DSPRingBufferMaxPosition;
end;

function TBeRoXM.Play:boolean;
begin
 Module.Reset;
 ResetSound;
 SetTickVariables;
 TickSamplesCounter:=0;
 BufferCounter:=0;
 OutputSamples:=0;
 Start:=true;
 Playing:=true;
 result:=Module.IsTrackActive;
{$ifndef android}
 if result then begin
  if Threaded then begin
   if ThreadIsSuspended and ThreadSuspended then begin
    ThreadSuspended:=false;
    SemaphorePost(ThreadResumeSemaphore);
    SemaphoreWait(ThreadActiveSemaphore);
   end else begin
    ThreadSuspended:=false;
    SemaphorePost(ThreadResumeSemaphore);
    while ThreadIsSuspended do begin
     sleep(10);
    end;
   end;
  end;
 end;
{$endif}
{$ifdef sdl}
 case SoundOutputMode of
  somSDL:begin
   SDL_PauseAudio(0);
  end;
 end;
{$endif}
end;

procedure TBeRoXM.Stop;
begin
{$ifdef sdl}
 case SoundOutputMode of
  somSDL:begin
   SDL_PauseAudio(1);
  end;
 end;
{$endif}
{$ifndef android}
 if Threaded then begin
  if not (ThreadIsSuspended or ThreadSuspended) then begin
   ThreadSuspended:=true;
   SemaphoreWait(ThreadSuspendSemaphore);
  end else begin
   ThreadSuspended:=true;
   while not ThreadIsSuspended do begin
    sleep(10);
   end;
  end;
 end;
{$endif}
 Playing:=false;
 ResetSound;
end;

function TBeRoXM.GetSampleLength(Channel:TBeRoXMChannel;CountSamplesValue:TBeRoXMMixerVariable):TBeRoXMMixerVariable;
var SmpLoopStart,SmpLoopEnd,SmpLen,SmpLenEx,CountSamples,MaxSamples,
    Difference:TBeRoXMMixerVariable;
begin
 SmpLen:=TBeRoXMMixerVariable(Channel.Sample.SampleLength)*TBeRoXMMixerVariable(BeRoXMPositionFactor);
 SmpLenEx:=TBeRoXMMixerVariable(Channel.Sample.SampleLength-1)*TBeRoXMMixerVariable(BeRoXMPositionFactor);
 if Channel.Sample.Loop then begin
  SmpLoopStart:=TBeRoXMMixerVariable(Channel.Sample.LoopStart)*TBeRoXMMixerVariable(BeRoXMPositionFactor);
  SmpLoopEnd:=TBeRoXMMixerVariable(Channel.Sample.LoopStart+Channel.Sample.LoopLength)*TBeRoXMMixerVariable(BeRoXMPositionFactor);
 end else begin
  SmpLoopStart:=0;
  SmpLoopEnd:=SmpLen;
 end;
 if Channel.SamplePosition<SmpLoopStart then begin
  if Channel.SampleBackwards then begin
   if Channel.Sample.PingPongLoop then begin
    Channel.SamplePosition:=SmpLoopStart-(SmpLoopStart-Channel.SamplePosition);
    Channel.SampleBackwards:=false;
    if (Channel.SamplePosition<SmpLoopStart) or (Channel.SamplePosition>=((SmpLoopStart+SmpLoopEnd) div 2)) then begin
     Channel.SamplePosition:=SmpLoopStart;
    end;
   end else if Channel.Sample.Loop then begin
    Channel.SamplePosition:=SmpLoopEnd-(SmpLoopStart-Channel.SamplePosition);
    if Channel.SamplePosition>=SmpLoopEnd then Channel.SamplePosition:=SmpLoopEnd;
   end else begin
    result:=0;
    exit;
   end;
  end else begin
   if Channel.SamplePosition<0 then begin
    dec(Channel.SamplePosition,(Channel.SamplePosition div BeRoXMPositionFactor)*BeRoXMPositionFactor);
   end;
  end;
 end else if Channel.SamplePosition>=SmpLoopEnd then begin
  if Channel.Sample.PingPongLoop then begin
   Channel.SamplePosition:=SmpLoopEnd-(Channel.SamplePosition-SmpLoopEnd);
   Channel.SampleBackwards:=true;
   if (Channel.SamplePosition<SmpLoopStart) or (Channel.SamplePosition>=SmpLoopEnd) then begin
    Channel.SamplePosition:=(Channel.SamplePosition mod BeRoXMPositionFactor)+SmpLenEx;
   end;
  end else if Channel.Sample.Loop then begin
   Channel.SamplePosition:=Channel.SamplePosition+(SmpLoopStart-SmpLoopEnd);
   if Channel.SamplePosition<SmpLoopStart then begin
    Channel.SamplePosition:=SmpLoopStart;
   end;
   Channel.SampleBackwards:=false;
  end else begin
   result:=0;
   exit;
  end;
 end;
 if (Channel.SamplePosition<0) or (Channel.SamplePosition>=SmpLen) or
    ((Channel.SamplePosition<SmpLoopStart) and
     ((Channel.SamplePosition<0) or Channel.SampleBackwards)) then begin
  result:=0;
  exit;
 end;
 CountSamples:=CountSamplesValue;
 MaxSamples:=BeRoXMPositionAllRemainFactor div ((Channel.Increment div BeRoXMPositionFactor)+1);
 if MaxSamples<2 then MaxSamples:=2;
 if CountSamplesValue>MaxSamples then CountSamplesValue:=MaxSamples;
 if Channel.SampleBackwards then begin
  Difference:=(Channel.SamplePosition-(Channel.Increment*(CountSamplesValue-1)));
  if Difference<SmpLoopStart then begin
   CountSamples:=((Channel.SamplePosition-SmpLoopStart-1) div Channel.Increment)+1;
  end;
 end else begin
  Difference:=(Channel.SamplePosition+(Channel.Increment*(CountSamplesValue-1)));
  if Difference>=SmpLoopEnd then begin
   CountSamples:=((SmpLoopEnd-Channel.SamplePosition-1) div Channel.Increment)+1;
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

procedure TBeRoXM.ProcessClickRemoval(StartPosition,LengthCounter:integer);
var Counter,LastLeft,LastRight:integer;
    Buf:plongint;
begin
 if OutputChannels=2 then begin
  LastLeft:=Module.LastLeft;
  LastRight:=Module.LastRight;
  if (LastLeft<>0) or (LastRight<>0) then begin
   Buf:=pointer(@Buffer^[StartPosition*OutputChannels]);
   for Counter:=0 to LengthCounter-1 do begin
{$ifdef HasSAR}
    dec(LastLeft,SARLongint(LastLeft+(SARLongint(-LastLeft,31) and $ff),8));
    dec(LastRight,SARLongint(LastRight+(SARLongint(-LastRight,31) and $ff),8));
{$else}
{$ifdef UseSAR}
    dec(LastLeft,sar(LastLeft+(sar(-LastLeft,31) and $ff),8));
    dec(LastRight,sar(LastRight+(sar(-LastRight,31) and $ff),8));
{$else}
    dec(LastLeft,(LastLeft+(((-LastLeft) div (1 shl 31)) and $ff)) div 256);
    dec(LastRight,(LastRight+(((-LastRight) div (1 shl 31)) and $ff)) div 256);
{$endif}
{$endif}
    Buf^:=Buf^+LastLeft;
    inc(Buf);
    Buf^:=Buf^+LastRight;
    inc(Buf);
   end;
   Module.LastLeft:=LastLeft;
   Module.LastRight:=LastRight;
  end;
 end else begin
  LastLeft:=Module.LastLeft;
  if LastLeft<>0 then begin
   Buf:=pointer(@Buffer^[StartPosition*OutputChannels]);
   for Counter:=0 to LengthCounter-1 do begin
{$ifdef HasSAR}
    dec(LastLeft,SARLongint(LastLeft+(sar(-LastLeft,31) and $ff),8));
{$else}
{$ifdef UseSAR}
    dec(LastLeft,sar(LastLeft+(sar(-LastLeft,31) and $ff),8));
{$else}
    dec(LastLeft,(LastLeft+(((-LastLeft) div (1 shl 31)) and $ff)) div 256);
{$endif}
{$endif}
    Buf^:=Buf^+LastLeft;
    inc(Buf);
   end;
   Module.LastLeft:=LastLeft;
  end;
 end;
end;

function TBeRoXM.DoMix(StartPosition,LengthCounter:integer;var DoContinue:boolean;NewTick:boolean):longword;
var TheLength,Counter:longword;
begin
 if NewTick then begin
 end;
 if (StartPosition+LengthCounter)<=BufferSize then begin
  TheLength:=LengthCounter;
 end else if StartPosition<=BufferSize then begin
  TheLength:=BufferSize-StartPosition;
 end else begin
  TheLength:=0;
 end;
 if TheLength>0 then begin
  dec(TickSamplesCounter,TheLength);
  for Counter:=low(TBeRoXMAudioProcessChains) to high(TBeRoXMAudioProcessChains) do begin
   if assigned(Module.AudioProcessChains[Counter]) then begin
    Module.AudioProcessChains[Counter].ResetBuffer(TheLength);
   end;
  end;
  for Counter:=0 to Module.NumberOfChannels-1 do begin
   MixChannel(Module.Channel[Counter],StartPosition,TheLength);
  end;
  for Counter:=0 to BeRoXMLastChannel do begin
   if Module.NewNoteActionChannel[Counter].Active then begin
    MixChannel(Module.NewNoteActionChannel[Counter],StartPosition,TheLength);
   end;
  end;
  for Counter:=0 to BeRoXMLastChannel do begin
   if Module.ClickRemovalFadeOutChannel[Counter].Active then begin
    MixChannel(Module.ClickRemovalFadeOutChannel[Counter],StartPosition,TheLength);
   end;
  end;
  for Counter:=low(TBeRoXMAudioProcessChains) to high(TBeRoXMAudioProcessChains) do begin
   if assigned(Module.AudioProcessChains[Counter]) then begin
    Module.AudioProcessChains[Counter].ProcessBuffer(StartPosition,TheLength);
   end;
  end;
  ProcessClickRemoval(StartPosition,TheLength);
 end else begin
  DoContinue:=false;
 end;
 result:=TheLength;
end;

procedure TBeRoXM.FillBuffer;
var Counter:integer;
    DoContinue,NewTick:boolean;
begin
{$ifdef ChannelOscillators}
 for Counter:=0 to Module.NumberOfChannels-1 do begin
  Module.Channel[Counter].OscillatorPosition:=0;
 end;
{$endif}
 Buffer:=MixBufferData;
 FillChar(Buffer^,BufferSize*sizeof(longint)*OutputChannels,#0);
 Counter:=0;
 DoContinue:=true;
 if Playing then begin
  while Counter<BufferSize do begin
   if TickSamplesCounter=0 then begin
    ProcessTick;
    NewTick:=true;
   end else begin
    NewTick:=false;
   end;
   inc(Counter,DoMix(Counter,TickSamplesCounter,DoContinue,NewTick));
  end;
 end;
end;

procedure TBeRoXM.MixTo(DestBuffer:pointer;Volume:longint);
var Buf,DestBuf:plongint;
    BufCounter,Count:integer;
begin
 Buf:=MixBufferData;
 DestBuf:=DestBuffer;
 Count:=BufferSize*OutputChannels;
{$ifdef UnrolledLoops}
 if Volume=4096 then begin
  for BufCounter:=1 to Count shr 2 do begin
   inc(DestBuf^,{$ifdef HasSAR}SARLongint(Buf^,BeRoXMOutBits){$else}SAROut(Buf^){$endif});
   inc(Buf);
   inc(DestBuf);
   inc(DestBuf^,{$ifdef HasSAR}SARLongint(Buf^,BeRoXMOutBits){$else}SAROut(Buf^){$endif});
   inc(Buf);
   inc(DestBuf);
   inc(DestBuf^,{$ifdef HasSAR}SARLongint(Buf^,BeRoXMOutBits){$else}SAROut(Buf^){$endif});
   inc(Buf);
   inc(DestBuf);
   inc(DestBuf^,{$ifdef HasSAR}SARLongint(Buf^,BeRoXMOutBits){$else}SAROut(Buf^){$endif});
   inc(Buf);
   inc(DestBuf);
  end;
  for BufCounter:=1 to Count and 3 do begin
   inc(DestBuf^,{$ifdef HasSAR}SARLongint(Buf^,BeRoXMOutBits){$else}SAROut(Buf^){$endif});
   inc(Buf);
   inc(DestBuf);
  end;
 end else begin
  for BufCounter:=1 to Count shr 2 do begin
   inc(DestBuf^,{$ifdef HasSAR}SARLongint{$else}SAR{$endif}({$ifdef HasSAR}SARLongint(Buf^,BeRoXMOutBits){$else}SAROut(Buf^){$endif}*Volume,12));
   inc(Buf);
   inc(DestBuf);
   inc(DestBuf^,{$ifdef HasSAR}SARLongint{$else}SAR{$endif}({$ifdef HasSAR}SARLongint(Buf^,BeRoXMOutBits){$else}SAROut(Buf^){$endif}*Volume,12));
   inc(Buf);
   inc(DestBuf);
   inc(DestBuf^,{$ifdef HasSAR}SARLongint{$else}SAR{$endif}({$ifdef HasSAR}SARLongint(Buf^,BeRoXMOutBits){$else}SAROut(Buf^){$endif}*Volume,12));
   inc(Buf);
   inc(DestBuf);
   inc(DestBuf^,{$ifdef HasSAR}SARLongint{$else}SAR{$endif}({$ifdef HasSAR}SARLongint(Buf^,BeRoXMOutBits){$else}SAROut(Buf^){$endif}*Volume,12));
   inc(Buf);
   inc(DestBuf);
  end;
  for BufCounter:=1 to Count and 3 do begin
   inc(DestBuf^,{$ifdef HasSAR}SARLongint{$else}SAR{$endif}({$ifdef HasSAR}SARLongint(Buf^,BeRoXMOutBits){$else}SAROut(Buf^){$endif}*Volume,12));
   inc(Buf);
   inc(DestBuf);
  end;
 end;
{$else}
 if Volume=4096 then begin
  for BufCounter:=1 to Count do begin
   inc(DestBuf^,{$ifdef HasSAR}SARLongint(Buf^,BeRoXMOutBits){$else}SAROut(Buf^){$endif});
   inc(Buf);
   inc(DestBuf);
  end;
 end else begin
  for BufCounter:=1 to Count do begin
   inc(DestBuf^,{$ifdef HasSAR}SARLongint{$else}SAR{$endif}({$ifdef HasSAR}SARLongint(Buf^,BeRoXMOutBits){$else}SAROut(Buf^){$endif}*Volume,12));
   inc(Buf);
   inc(DestBuf);
  end;
 end;
{$endif}
end;

procedure TBeRoXM.DownMix(DestBuffer:pointer);
var Count,BufCounter,Value:integer;
    Buf:plongint;
    DestBuf:psmallint;
    DestBuf8Bit:pbyte;
begin
 Buf:=pointer(@Buffer[0]);
 DestBuf:=DestBuffer;
 Count:=BufferSize*OutputChannels;
{$ifdef UnrolledLoops}
{$ifdef HasSAR}
 if Clipping then begin
  for BufCounter:=1 to Count shr 2 do begin
   Value:=SARLongint(Buf^,BeRoXMOutBits);
   DestBuf^:=SARLongint(longint((abs(Value+32768)-1)-abs(Value-32767)),1);
   inc(Buf);
   inc(DestBuf);
   Value:=SARLongint(Buf^,BeRoXMOutBits);
   DestBuf^:=SARLongint(longint((abs(Value+32768)-1)-abs(Value-32767)),1);
   inc(Buf);
   inc(DestBuf);
   Value:=SARLongint(Buf^,BeRoXMOutBits);
   DestBuf^:=SARLongint(longint((abs(Value+32768)-1)-abs(Value-32767)),1);
   inc(Buf);
   inc(DestBuf);
   Value:=SARLongint(Buf^,BeRoXMOutBits);
   DestBuf^:=SARLongint(longint((abs(Value+32768)-1)-abs(Value-32767)),1);
   inc(Buf);
   inc(DestBuf);
  end;
  for BufCounter:=1 to Count and 3 do begin
   Value:=SARLongint(Buf^,BeRoXMOutBits);
   DestBuf^:=SARLongint(longint((abs(Value+32768)-1)-abs(Value-32767)),1);
   inc(Buf);
   inc(DestBuf);
  end;
 end else begin
  for BufCounter:=1 to Count shr 2 do begin
   DestBuf^:=SARLongint(Buf^,BeRoXMOutBits);
   inc(Buf);
   inc(DestBuf);
   DestBuf^:=SARLongint(Buf^,BeRoXMOutBits);
   inc(Buf);
   inc(DestBuf);
   DestBuf^:=SARLongint(Buf^,BeRoXMOutBits);
   inc(Buf);
   inc(DestBuf);
   DestBuf^:=SARLongint(Buf^,BeRoXMOutBits);
   inc(Buf);
   inc(DestBuf);
  end;
  for BufCounter:=1 to Count and 3 do begin
   DestBuf^:=SARLongint(Buf^,BeRoXMOutBits);
   inc(Buf);
   inc(DestBuf);
  end;
 end;
{$else}
{$ifdef UseSAR}
 if Clipping then begin
  for BufCounter:=1 to Count shr 2 do begin
   Value:=SAROut(Buf^);
   DestBuf^:=SAR1((abs(Value+32768)-1)-abs(Value-32767));
   inc(Buf);
   inc(DestBuf);
   Value:=SAROut(Buf^);
   DestBuf^:=SAR1((abs(Value+32768)-1)-abs(Value-32767));
   inc(Buf);
   inc(DestBuf);
   Value:=SAROut(Buf^);
   DestBuf^:=SAR1((abs(Value+32768)-1)-abs(Value-32767));
   inc(Buf);
   inc(DestBuf);
   Value:=SAROut(Buf^);
   DestBuf^:=SAR1((abs(Value+32768)-1)-abs(Value-32767));
   inc(Buf);
   inc(DestBuf);
  end;
  for BufCounter:=1 to Count and 3 do begin
   Value:=SAROut(Buf^);
   DestBuf^:=SAR1((abs(Value+32768)-1)-abs(Value-32767));
   inc(Buf);
   inc(DestBuf);
  end;
 end else begin
  for BufCounter:=1 to Count shr 2 do begin
   DestBuf^:=SAROut(Buf^);
   inc(Buf);
   inc(DestBuf);
   DestBuf^:=SAROut(Buf^);
   inc(Buf);
   inc(DestBuf);
   DestBuf^:=SAROut(Buf^);
   inc(Buf);
   inc(DestBuf);
   DestBuf^:=SAROut(Buf^);
   inc(Buf);
   inc(DestBuf);
  end;
  for BufCounter:=1 to Count and 3 do begin
   DestBuf^:=SAROut(Buf^);
   inc(Buf);
   inc(DestBuf);
  end;
 end;
{$else}
 if Clipping then begin
  for BufCounter:=1 to Count shr 2 do begin
   Value:=Buf^ div BeRoXMOutLength;
   DestBuf^:=longint(longint((abs(Value+32768)-1)-abs(Value-32767)) div 2);
   inc(Buf);
   inc(DestBuf);
   Value:=Buf^ div BeRoXMOutLength;
   DestBuf^:=longint(longint((abs(Value+32768)-1)-abs(Value-32767)) div 2);
   inc(Buf);
   inc(DestBuf);
   Value:=Buf^ div BeRoXMOutLength;
   DestBuf^:=longint(longint((abs(Value+32768)-1)-abs(Value-32767)) div 2);
   inc(Buf);
   inc(DestBuf);
   Value:=Buf^ div BeRoXMOutLength;
   DestBuf^:=longint(longint((abs(Value+32768)-1)-abs(Value-32767)) div 2);
   inc(Buf);
   inc(DestBuf);
  end;
  for BufCounter:=1 to Count and 3 do begin
   Value:=Buf^ div BeRoXMOutLength;
   DestBuf^:=longint(longint((abs(Value+32768)-1)-abs(Value-32767)) div 2);
   inc(Buf);
   inc(DestBuf);
  end;
 end else begin
  for BufCounter:=1 to Count shr 2 do begin
   DestBuf^:=longint(longint(Buf^) div BeRoXMOutLength);
   inc(Buf);
   inc(DestBuf);
   DestBuf^:=longint(longint(Buf^) div BeRoXMOutLength);
   inc(Buf);
   inc(DestBuf);
   DestBuf^:=longint(longint(Buf^) div BeRoXMOutLength);
   inc(Buf);
   inc(DestBuf);
   DestBuf^:=longint(longint(Buf^) div BeRoXMOutLength);
   inc(Buf);
   inc(DestBuf);
  end;
  for BufCounter:=1 to Count and 3 do begin
   DestBuf^:=longint(longint(Buf^) div BeRoXMOutLength);
   inc(Buf);
   inc(DestBuf);
  end;
 end;
{$endif}
{$endif}
{$else}
{$ifdef HasSAR}
 if Clipping then begin
  for BufCounter:=1 to Count do begin
   Value:=SARLongint(Buf^,BeRoXMOutBits);
   DestBuf^:=SARLongint(longint((abs(Value+32768)-1)-abs(Value-32767)),1);
   inc(Buf);
   inc(DestBuf);
  end;
 end else begin
  for BufCounter:=1 to Count do begin
   DestBuf^:=SARLongint(Buf^,BeRoXMOutBits);
   inc(Buf);
   inc(DestBuf);
  end;
 end;
{$else}
{$ifdef UseSAR}
 if Clipping then begin
  for BufCounter:=1 to Count do begin
   Value:=SAROut(Buf^);
   Value:=SAR1((abs(Value+32768)-1)-abs(Value-32767));
   DestBuf^:=Value;
   inc(Buf);
   inc(DestBuf);
  end;
 end else begin
  for BufCounter:=1 to Count do begin
   Value:=SAROut(Buf^);
   inc(Buf);
   inc(DestBuf);
  end;
 end;
{$else}
 if Clipping then begin
  for BufCounter:=1 to Count do begin
   Value:=longint(longint(Buf^) div BeRoXMOutLength);
   DestBuf^:=longint(longint((abs(Value+32768)-1)-abs(Value-32767)) div 2);
   inc(Buf);
   inc(DestBuf);
  end;
 end else begin
  for BufCounter:=1 to Count do begin
   DestBuf^:=longint(longint(Buf^) div BeRoXMOutLength);
   inc(Buf);
   inc(DestBuf);
  end;
 end;
{$endif}
{$endif}
{$endif}
 if OutputBits=8 then begin
  DestBuf:=DestBuffer;
  DestBuf8Bit:=pointer(DestBuf);
  for BufCounter:=1 to Count shr 2 do begin
   DestBuf8Bit^:=(DestBuf^+32768) shr 8;
   inc(DestBuf);
   inc(DestBuf8Bit);
   DestBuf8Bit^:=(DestBuf^+32768) shr 8;
   inc(DestBuf);
   inc(DestBuf8Bit);
   DestBuf8Bit^:=(DestBuf^+32768) shr 8;
   inc(DestBuf);
   inc(DestBuf8Bit);
   DestBuf8Bit^:=(DestBuf^+32768) shr 8;
   inc(DestBuf);
   inc(DestBuf8Bit);
  end;
  for BufCounter:=1 to Count and 3 do begin
   DestBuf8Bit^:=(DestBuf^+32768) shr 8;
   inc(DestBuf);
   inc(DestBuf8Bit);
  end;
 end;
end;

procedure TBeRoXM.IncOutputSamples;
{$ifndef cpu386}
var NewOutputSamples:int64;
{$endif}
begin
{$ifdef cpu386}
 asm
  push ebx
  push ecx
  push edx
   mov ebx,dword ptr self
   mov edx,dword ptr [ebx+TBeRoXM.BufferSamples]
   lea ecx,dword ptr [ebx+TBeRoXM.OutputSamples]
   lock add dword ptr [ecx],edx
   lock adc dword ptr [ecx+4],0
  pop edx
  pop ecx
  pop ebx
 end;
{$else}
 NewOutputSamples:=OutputSamples+BufferSamples;
 OutputSamples:=NewOutputSamples;
{$endif}
end;

procedure TBeRoXM.GetBuffer(DestBuffer:pointer;Bytes:longword);
var Remain,ToDo,DestBufferPosition:longword;
begin
 if Playing then begin
  DestBufferPosition:=0;
  Remain:=Bytes;
  while Remain>0 do begin
   if DSPRingBufferPosition>=DSPRingBufferMaxPosition then begin
    DSPRingBufferPosition:=0;
    if assigned(DSPRingBuffer) then begin
     FillBuffer;
     DownMix(DSPRingBuffer);
     IncOutputSamples;
    end;
   end else if DSPRingBufferPosition<DSPRingBufferMaxPosition then begin
    ToDo:=DSPRingBufferMaxPosition-DSPRingBufferPosition;
    if ToDo>Remain then begin
     ToDo:=Remain;
    end;
    if ToDo>0 then begin
     if assigned(DSPRingBuffer) and assigned(DestBuffer) then begin
      move(pchar(DSPRingBuffer)[DSPRingBufferPosition],pchar(DestBuffer)[DestBufferPosition],ToDo);
     end;
     inc(DSPRingBufferPosition,ToDo);
     inc(DestBufferPosition,ToDo);
     dec(Remain,ToDo);
    end else begin
     break;
    end;
   end else begin
    break;
   end;
  end;
 end else begin
  if OutputBits=8 then begin
   FillChar(DestBuffer^,Bytes,#128);
  end else begin
   FillChar(DestBuffer^,Bytes,#0);
  end;
 end;
end;

procedure TBeRoXM.GetBufferSamples(DestBuffer:pointer;Samples:longword);
begin
 GetBuffer(DestBuffer,Samples*longword(BufferMul));
end;

procedure TBeRoXM.MixBuffer(DestBuffer:pointer);
begin
 FillBuffer;
 DownMix(DestBuffer);
end;

procedure TBeRoXM.Poll;
{$ifdef UseDirectSound}
var CurrentBuffer,CurrentSamplePosition:integer;
    P1,P2:pointer;
    S1,S2:longword;
{$endif}
begin
 case SoundOutputMode of
  somNONE:begin
  end;
{$ifdef windows}
  somWaveOut:begin
   while (BuffersPrebuffered<PrebufferCount) and not ThreadTerminated do begin
    if (WaveHandler[BufferCounter].dwFlags and WHDR_DONE)<>0 then begin
     if waveOutUnprepareHeader(WaveOutHandle,WaveHandler[BufferCounter],sizeof(TWAVEHDR))<>WAVERR_STILLPLAYING then begin
      if WaveOutFirstBufferTime<0 then begin
       WaveOutFirstBufferTime:=timeGetTime;
      end;
      WaveHandler[BufferCounter].dwFlags:=WaveHandler[BufferCounter].dwFlags and not WHDR_DONE;
      FillBuffer;
      DownMix(WaveHandler[BufferCounter].lpData);
      waveOutPrepareHeader(WaveOutHandle,WaveHandler[BufferCounter],sizeof(TWAVEHDR));
      waveOutWrite(WaveOutHandle,WaveHandler[BufferCounter],sizeof(TWAVEHDR));
      inc(BufferCounter);
      if BufferCounter>=BufferCount then BufferCounter:=0;
      IncOutputSamples;
      inc(BuffersPrebuffered);
     end;
    end;
   end;
   if (WaveHandler[BufferCounter].dwFlags and WHDR_DONE)<>0 then begin
    if waveOutUnprepareHeader(WaveOutHandle,WaveHandler[BufferCounter],sizeof(TWAVEHDR))<>WAVERR_STILLPLAYING then begin
     if WaveOutFirstBufferTime<0 then begin
      WaveOutFirstBufferTime:=timeGetTime;
     end;
     WaveHandler[BufferCounter].dwFlags:=WaveHandler[BufferCounter].dwFlags and not WHDR_DONE;
     FillBuffer;
     DownMix(WaveHandler[BufferCounter].lpData);
     waveOutPrepareHeader(WaveOutHandle,WaveHandler[BufferCounter],sizeof(TWAVEHDR));
     waveOutWrite(WaveOutHandle,WaveHandler[BufferCounter],sizeof(TWAVEHDR));
     inc(BufferCounter);
     if BufferCounter>=BufferCount then BufferCounter:=0;
     IncOutputSamples;
    end;
   end;
  end;
{$ifdef UseDirectSound}
  somDirectSound:begin
   DirectSoundBuffer.GetCurrentPosition(@CurrentSamplePosition,nil);
   CurrentBuffer:=CurrentSamplePosition div OutputBufferSize;
   if (BufferCounter<>CurrentBuffer) and
      (BufferCounter>=0) and (BufferCounter<BufferCount) then begin
    DirectSoundBuffer.Lock(BufferCounter*OutputBufferSize,OutputBufferSize,P1,S1,P2,S2,0);
    if (S1<>0) and assigned(P1) then begin
     MixBuffer(P1);
    end;
    if BufferCounter=0 then begin
     DirectSoundBasePosition:=OutputSamples;
    end;
    DirectSoundBuffer.Unlock(P1,S1,P2,S2);
    inc(BufferCounter);
    if BufferCounter>=BufferCount then BufferCounter:=0;
    IncOutputSamples;
   end;
  end;
{$endif}
{$endif}
{$ifdef unix}
{$ifndef android}
  somDevDSP:begin
   while BuffersPrebuffered<PrebufferCount do begin
    MixBuffer(DevDSPBuffer);
    __write(DevDSPFileHandle,DevDSPBuffer,OutputBufferSize);
    inc(BuffersPrebuffered);
    IncOutputSamples;
   end;
   MixBuffer(DevDSPBuffer);
   __write(DevDSPFileHandle,DevDSPBuffer,OutputBufferSize);
   IncOutputSamples;
  end;
{$endif}
{$endif}
{$ifdef sdl}
  somSDL:begin
{$ifdef sdlex}
   MixBuffer(DSPRingBuffer);
   IncOutputSamples;
   while RingBufferSpace(RingBuffer)<longword(OutputBufferSize) do begin
    sleep(1);
   end;
   WriteToRingBuffer(RingBuffer,pointer(DSPRingBuffer),OutputBufferSize);
{$else}
   sleep(50);
{$endif}
  end;
{$endif}
{$ifdef BeRoXMRingBuffer}
  somRingBuffer:begin
   sleep(50);
  end;
{$endif}
 end;
end;

function TBeRoXM.GetTimePosition:int64;
(*{$ifdef windows}
var MMTime:TMMTime;
{$ifdef UseDirectSound}
    CurrentSamplePosition:integer;
{$endif}
{$endif}
{$ifdef windows}
 function GetWaveOutTimeEx:int64;
 begin
  result:=timeGetTime-WaveOutFirstBufferTime;
  if result<0 then begin
   result:=0;
  end;
 end;
 function GetWaveOutTime:int64;
 begin
  if WaveOutIsSampleAccurate then begin
   MMTime.wType:=TIME_SAMPLES;
   if (WaveOutGetPosition(WaveOutHandle,@MMTime,sizeof(TMMTime))=MMSYSERR_NOERROR) and
      (MMTime.wType=TIME_SAMPLES) then begin
    if MMTime.sample<WaveOutLastGetPositionSample then begin
     if WaveOutLastGetPositionSample<=$7ffffff then begin
      inc(WaveOutLastGetPositionSample,$7ffffff);
     end else begin
      inc(WaveOutAddGetPositionSample,WaveOutLastGetPositionSample);
      if MMTime.sample<=ptruint(BufferSamples) then begin
       inc(WaveOutAddGetPositionSample,BufferSamples);
       dec(WaveOutAddGetPositionSample,MMTime.sample);
      end;
     end;
    end;
    WaveOutLastGetPositionSample:=MMTime.sample;
    result:=MMTime.sample+WaveOutAddGetPositionSample;
    result:=((result*1000)+(WaveFormat.nSamplesPerSec div 2)) div WaveFormat.nSamplesPerSec;
    exit;
   end;
  end;

  MMTime.wType:=TIME_MS;
  if (WaveOutGetPosition(WaveOutHandle,@MMTime,sizeof(TMMTime))=MMSYSERR_NOERROR) and
     (MMTime.wType=TIME_MS) then begin
   result:=MMTime.ms;
   exit;
  end;

  MMTime.wType:=TIME_BYTES;
  if (WaveOutGetPosition(WaveOutHandle,@MMTime,sizeof(TMMTime))=MMSYSERR_NOERROR) and
     (MMTime.wType=TIME_BYTES) then begin
   result:=(MMTime.cb+(WaveFormat.nBlockAlign div 2)) div WaveFormat.nBlockAlign;
   result:=((result*1000)+(WaveFormat.nSamplesPerSec div 2)) div WaveFormat.nSamplesPerSec;
   exit;
  end;

  MMTime.wType:=TIME_SMPTE;
  if (WaveOutGetPosition(WaveOutHandle,@MMTime,sizeof(TMMTime))=MMSYSERR_NOERROR) and
     (MMTime.wType=TIME_SMPTE) and (MMTime.FPS<>0) then begin
   result:=MMTime.hour;
   result:=(result*60)+MMTime.min;
   result:=(result*60)+MMTime.sec;
   result:=(result*1000)+(((MMTime.Frame*int64(1000))+(MMTime.FPS div 2)) div MMTime.FPS);
   exit;
  end;

  result:=GetWaveOutTimeEx;
 end;
{$endif}*)
begin
 case SoundOutputMode of
  somNone:begin
   result:=0;
  end;
{$ifdef windows}
  somWaveOut:begin
   result:=0;//GetWaveOutTime;
  end;
{$ifdef UseDirectSound}
 somDirectSound:begin
   DirectSoundBuffer.GetCurrentPosition(@CurrentSamplePosition,nil);
   result:=DirectSoundBasePosition+CurrentSamplePosition;
  end;
{$endif}
{$endif}
{$ifdef unix}
{$ifndef android}
{ somDevDSP:begin
  end;}
{$endif}
{$endif}
{$ifdef sdl}
{ somSDL:begin
  end;}
{$endif}
{$ifdef BeRoXMRingBuffer}
{ somRingBuffer:begin
  end;}
{$endif}
  else begin
   result:=((OutputSamples*1000)+(OutputSampleRate div 2)) div int64(OutputSampleRate+0);
  end;
 end;
end;

function TBeRoXM.GetTimeDuration(Factor:int64=1;Position:int64=-1):int64;
type PPatternPlayedCounter=^TPatternPlayedCounter;
     TPatternPlayedCounter=array[0..BeRoXMLastPatternOrder] of integer;
     PLoopPatternRow=^TLoopPatternRow;
     TLoopPatternRow=array[0..BeRoXMLastChannel] of byte;
     PLoopCount=^TLoopCount;
     TLoopCount=array[0..BeRoXMLastChannel] of byte;
     PRowCounter=^TRowCounter;
     TRowCounter=array[0..BeRoXMLastPatternRow] of integer;
var TrackEnd:boolean;
    Speed:integer;
    Tempo:integer;
    PatternDelay:integer;
    CurrentTickCounter:integer;
    CurrentPattern:integer;
    CurrentPatternRow:integer;
    NextPatternRow:integer;
    CurrentPatternOrder:integer;
    NextPatternOrder:integer;
    Duration,i60:int64;
    PatternPlayedCounter:PPatternPlayedCounter;
    LoopPatternRow:PLoopPatternRow;
    LoopCount:PLoopCount;
    RowCounter:PRowCounter;
    LastRowCounterPattern:integer;
    Looped:boolean;
 procedure InitCheckRow;
 begin
  LastRowCounterPattern:=CurrentPatternOrder;
  FillChar(RowCounter^,sizeof(TRowCounter),#0);
 end;
 procedure CheckRow;
 var PatternRows:integer;
 begin
  if LastRowCounterPattern<>CurrentPatternOrder then begin
   InitCheckRow;
  end;
  if (CurrentPatternRow>=0) and (CurrentPatternRow<=BeRoXMLastPatternRow) then begin
   inc(RowCounter^[CurrentPatternRow]);
   PatternRows:=64;
   if (CurrentPattern>=0) and (CurrentPattern<=BeRoXMLastPattern) then begin
    if assigned(Module.Pattern[CurrentPattern and $ff]) then begin
     PatternRows:=Module.Pattern[CurrentPattern and $ff].Rows;
    end;
   end;
   if RowCounter^[CurrentPatternRow]>PatternRows then begin
    TrackEnd:=true;
   end;
  end;
 end;
 function NextTick:boolean;
 begin
  result:=true;
  inc(CurrentTickCounter);
  if CurrentTickCounter>=(Speed*(PatternDelay+1)) then begin
   PatternDelay:=0;
   CurrentTickCounter:=0;
   CurrentPatternRow:=NextPatternRow;

   // Reset pattern loop effect
   if CurrentPatternOrder<>NextPatternOrder then begin
    CurrentPatternOrder:=NextPatternOrder;
    if CurrentPatternOrder<=BeRoXMLastPatternOrder then begin
     if PatternPlayedCounter^[CurrentPatternOrder]<>0 then begin
      TrackEnd:=true;
      if Looping then begin
       Looped:=true;
      end;
     end;
     inc(PatternPlayedCounter^[CurrentPatternOrder]);
    end;
   end;

   // Pattern valid?
   if (CurrentPatternOrder<=BeRoXMLastPatternOrder) and (CurrentPatternOrder<Module.TrackLength) then begin
    CurrentPattern:=Module.PatternOrder[CurrentPatternOrder];
   end else begin
    CurrentPattern:=-1;
   end;
   if ((CurrentPattern>=0) and (CurrentPattern<=BeRoXMLastPattern)) and not assigned(Module.Pattern[CurrentPattern and $ff].Pattern) then begin
    CurrentPattern:=-2;
   end;
   while CurrentPattern>BeRoXMLastPattern do begin
    // End of track?
    if (CurrentPattern<0) or ((CurrentPatternOrder>BeRoXMLastPatternOrder) or (CurrentPatternOrder>=Module.TrackLength)) then begin
     TrackEnd:=true;
     if Looping then begin
      Looped:=true;
     end;
     exit;
    end else begin
     inc(CurrentPatternOrder);
    end;
    if (CurrentPatternOrder<=BeRoXMLastPatternOrder) and (CurrentPatternOrder<Module.TrackLength) then begin
     CurrentPattern:=Module.PatternOrder[CurrentPatternOrder];
    end else begin
     CurrentPattern:=-1;
    end;
    if ((CurrentPattern>=0) and (CurrentPattern<=BeRoXMLastPattern)) and not assigned(Module.Pattern[CurrentPattern and $ff].Pattern) then begin
     CurrentPattern:=-2;
    end;
   end;
   NextPatternOrder:=CurrentPatternOrder;
   if ((CurrentPattern<0) or (CurrentPattern>BeRoXMLastPattern)) or not assigned(Module.Pattern[CurrentPattern and $ff].Pattern) then begin
    result:=false;
    exit;
   end;
   if CurrentPatternRow>=Module.Pattern[CurrentPattern and $ff].Rows then begin
    CurrentPatternRow:=0;
   enD;
   NextPatternRow:=CurrentPatternRow+1;
   if NextPatternRow>=Module.Pattern[CurrentPattern and $ff].Rows then begin
    NextPatternOrder:=CurrentPatternOrder+1;
    NextPatternRow:=0;
   end;
   CheckRow;
  end;
  if Speed=0 then begin
   Speed:=1;
  end;
 end;
 procedure ProcessPatternRow;
 var ANote:TBeRoXMPatternNote;
     ChannelNumber:integer;
     PatternLoopRow:integer;
     PatternJump:integer;
     BreakRow:integer;
     EffectParameter:byte;
 begin
  if (CurrentPattern>=0) and (CurrentPattern<=BeRoXMLastPattern) then begin
   BreakRow:=-1;
   PatternJump:=-1;
   PatternLoopRow:=-1;
   for ChannelNumber:=0 to Module.Pattern[CurrentPattern].Channels-1 do begin
    if true then begin
     ANote:=Module.Pattern[CurrentPattern].GetNote(CurrentPatternRow,ChannelNumber);
     EffectParameter:=ANote.EffectParameter;
     case ANote.Effect of
      BeRoXMEffectSpeedTempo:begin
       if CurrentTickCounter=0 then begin
        if EffectParameter<$20 then begin
         Speed:=EffectParameter;
        end else begin
         Tempo:=EffectParameter;
        end;
       end;
      end;
      BeRoXMEffectPatternJump:begin
       PatternJump:=EffectParameter;
      end;
      BeRoXMEffectPatternBreak:begin
       BreakRow:=EffectParameter;
      end;
     end;
     if ANote.Effect=BeRoXMEffectExtendedEffects then begin
      if (EffectParameter shr 4)=BeRoXMEffectExtendedEffectNoteDelay then begin // Note Delay ?
      end else if CurrentTickCounter=0 then begin
       // Pattern Loop ?
       if (EffectParameter shr 4)=BeRoXMEffectExtendedEffectPatternLoop then begin
        if (EffectParameter and $f)<>0 then begin
         if LoopCount^[ChannelNumber]<>0 then begin
          dec(LoopCount^[ChannelNumber]);
          if LoopCount^[ChannelNumber]>0 then begin
           PatternLoopRow:=LoopPatternRow^[ChannelNumber];
          end;
         end else begin
          LoopCount^[ChannelNumber]:=EffectParameter and $f;
          PatternLoopRow:=LoopPatternRow^[ChannelNumber];
         end;
        end else begin
         LoopPatternRow^[ChannelNumber]:=CurrentPatternRow;
        end;
       end else if (EffectParameter shr 4)=BeRoXMEffectExtendedEffectPatternDelay then begin // Pattern Delay
        PatternDelay:=EffectParameter and $f;
       end;
      end;
     end;
    end;
   end;
   if CurrentTickCounter=0 then begin
    if PatternLoopRow>=0 then begin
     NextPatternOrder:=CurrentPatternOrder;
     NextPatternRow:=PatternLoopRow;
     if PatternDelay<>0 then begin
      inc(NextPatternRow);
     end;
    end else if (BreakRow>=0) or (PatternJump>=0) then begin
     if PatternJump<0 then begin
      PatternJump:=CurrentPatternOrder+1;
     end;
     if BreakRow<0 then begin
      BreakRow:=0;
     end;
     if (Looping and (PatternJump<=BeRoXMLastPatternOrder)) and ((PatternJump<>CurrentPatternOrder) or (BreakRow<>CurrentPatternRow)) then begin
      NextPatternOrder:=PatternJump;
      NextPatternRow:=BreakRow;
      if NextPatternOrder=CurrentPatternOrder then begin
       if PatternPlayedCounter^[CurrentPatternOrder]>=Module.Pattern[CurrentPattern and $ff].Rows then begin
        TrackEnd:=true;
       end;
       inc(PatternPlayedCounter^[CurrentPatternOrder]);
      end;
      if NextPatternOrder>=Module.TrackLength then begin
       NextPatternOrder:=Module.RestartPosition;
       if NextPatternOrder>=Module.TrackLength then begin
        NextPatternOrder:=0;
       end;
       TrackEnd:=true;
       if Looping then begin
        Looped:=true;
       end;
      end;
     end;
    end;
   end;
  end;
  if Tempo=0 then begin
   Tempo:=1;
  end;
 end;
var i64:int64;
    Found:boolean;
begin
 New(PatternPlayedCounter);
 New(LoopPatternRow);
 New(LoopCount);
 New(RowCounter);
 try
  FillChar(PatternPlayedCounter^,sizeof(TPatternPlayedCounter),#0);
  FillChar(LoopPatternRow^,sizeof(TLoopPatternRow),#0);
  FillChar(LoopCount^,sizeof(TLoopCount),#0);
  FillChar(RowCounter^,sizeof(TRowCounter),#0);
  TrackEnd:=false;
  Looped:=false;
  Speed:=Module.StartSpeed;
  Tempo:=Module.StartTempo;
  PatternDelay:=0;
  CurrentTickCounter:=Speed;
  CurrentPattern:=0;
  CurrentPatternRow:=0;
  NextPatternRow:=0;
  CurrentPatternOrder:=0;
  NextPatternOrder:=0;
  Duration:=0;
  i60:=int64(60) shl 32;
  i60:=i60*Factor;
  InitCheckRow;
  if Position<0 then begin
   while NextTick and not TrackEnd do begin
    ProcessPatternRow;
    Duration:=Duration+(i60 div (Tempo*24));
   end;
  end else begin
   i64:=Position;
   i64:=i64 shl 32;
   Found:=false;
   while NextTick and not TrackEnd do begin
    ProcessPatternRow;
    Duration:=Duration+(i60 div (Tempo*24));
    if Duration>=i64 then begin
     Module.Tick:=CurrentTickCounter;
     Module.CurrentPattern:=CurrentPattern;
     Module.CurrentPatternRow:=CurrentPatternRow;
     Module.NextPatternRow:=NextPatternRow;
     Module.CurrentPatternOrder:=CurrentPatternOrder;
     Module.NextPatternOrder:=NextPatternOrder;
     Found:=true;
     break;
    end;
   end;
   Module.TrackEnd:=TrackEnd and not Looping;
   if (Looped or not Found) and Looping and (Module.RestartPosition=0) then begin
    Duration:=0;
    Module.CurrentPattern:=0;
    Module.CurrentPatternRow:=0;
    Module.NextPatternRow:=1;
    Module.CurrentPatternOrder:=0;
    Module.NextPatternOrder:=0;
    Module.Reset;
   end;
  end;
{$ifdef android}
  if Factor=44100 then begin
   OutputSamples:=Duration shr 32;
  end else begin
   OutputSamples:=((Duration*int64(44100)) div Factor) shr 32;
  end;
{$endif}
  Duration:=Duration shr 32;
  result:=Duration;
 finally
  Dispose(PatternPlayedCounter);
  Dispose(LoopPatternRow);
  Dispose(LoopCount);
  Dispose(RowCounter);
 end;
end;

function TBeRoXM.SeekToTimePosition(Factor:int64=1;Position:int64=-1):int64;
begin
 result:=GetTimeDuration(Factor,Position);
end;

{$I BeRoXMMixer.inc}

procedure TBeRoXM.MixChannel(Channel:TBeRoXMChannel;StartPosition,LengthCounter:integer);
var Counter,Remain{$ifdef ChannelOscillators},SubCounter{$endif}:integer;
    Buf,LastSampleBuffer:plongint;
    IsVolumeEnvelopeAtEnd:boolean;
    MixSampleProc:TDoMixSampleProc;
    ResamplingTable:pointer;
    ResamplingCounter,InChannelCounter,RampingCounter,FilterCounter,PanningCounter,OscillatorCounter:integer;
    AudioProcessChain:TBeRoXMAudioProcessChain;
 procedure ReselectMixSampleProc;
 begin
  if (Channel.StepVolumeRampingLeft=0) and (Channel.StepVolumeRampingRight=0) then begin
   RampingCounter:=BeRoXMMixerNoRamping;
  end else begin
   RampingCounter:=BeRoXMMixerRamping;
  end;
  case OutputChannels of
{$ifdef UseMono}
   1:MixSampleProc:=MonoDoMixSampleProcs[ResamplingCounter,InChannelCounter,RampingCounter,FilterCounter,OscillatorCounter];
{$endif}
{$ifdef UseStereo}
   2:begin
    if (Channel.StepVolumeRampingLeft=Channel.StepVolumeRampingRight) and
       (Channel.VolumeRampingLeft=Channel.VolumeRampingRight) and
       (Channel.DestVolumeRampingLeft=Channel.DestVolumeRampingRight) then begin
     PanningCounter:=BeRoXMMixerMonoPanning;
    end else begin
     PanningCounter:=BeRoXMMixerStereoPanning;
    end;
    MixSampleProc:=StereoDoMixSampleProcs[ResamplingCounter,InChannelCounter,RampingCounter,FilterCounter,PanningCounter,OscillatorCounter];
   end;
{$endif}
   else MixSampleProc:=nil;
  end;
 end;
 procedure SelectMixSampleProc;
 begin
  ResamplingCounter:=ResamplingMethod;
  case ResamplingCounter of
{$ifdef MixerCubicSpline}
   BeRoXMMixerCubicSpline:ResamplingTable:=@CubicSpline.Table;
{$else}
   BeRoXMMixerCubicSpline:ResamplingTable:=nil;
{$endif}
{$ifdef MixerWindowedFIR}
   BeRoXMMixerWindowedFIR:ResamplingTable:=@WindowedFIR.Table;
{$else}
   BeRoXMMixerWindowedFIR:ResamplingTable:=nil;
{$endif}
   else ResamplingTable:=nil;
  end;
{$ifdef MixerNearest}
  if Channel.Increment=BeRoXMPositionFactor then begin
   ResamplingCounter:=BeRoXMMixerNearest;
  end;
{$endif}
  case Channel.Sample.Channels of
   1:InChannelCounter:=BeRoXMMixerInMono;
   2:InChannelCounter:=BeRoXMMixerInStereo;
   else InChannelCounter:=BeRoXMMixerInMono;
  end;
  if Channel.FilterActive then begin
   case Channel.FilterMode of
    BeRoXMCutOffLowPass:FilterCounter:=BeRoXMMixerLowPass;
    BeRoXMCutOffHighPass:FilterCounter:=BeRoXMMixerHighPass;
    else FilterCounter:=BeRoXMMixerNoFilter;
   end;
  end else begin
   FilterCounter:=BeRoXMMixerNoFilter;
  end;
{$ifdef ChannelOscillators}
  if Oscillators then begin
   OscillatorCounter:=BeRoXMMixerWithOscillator;
  end else begin
   OscillatorCounter:=BeRoXMMixerWithoutOscillator;
  end;
  OscillatorCounter:=BeRoXMMixerWithOscillator;
{$else}
  OscillatorCounter:=BeRoXMMixerWithoutOscillator;
{$endif}
  ReselectMixSampleProc;
 end;
{$ifdef MixerIntegerIncrement}
 procedure ConvertRealToFast;
 begin
  Channel.SampleIntegerPosition:=Channel.SamplePosition div BeRoXMPositionFactor;
  Channel.SampleIntegerSubPosition:=Channel.SamplePosition mod BeRoXMPositionFactor;
  Channel.SampleIntegerIncrement:=Channel.SampleIncrement;
 end;
 procedure ConvertFastToReal;
 begin
  Channel.SamplePosition:=(Channel.SampleIntegerPosition*BeRoXMPositionFactor)+Channel.SampleIntegerSubPosition;
 end;
{$endif}
begin
 if Channel.Increment=0 then begin
  Channel.Active:=false;
 end else if Channel.Active and assigned(Channel.Sample) then begin
  SelectMixSampleProc;
  if (Channel.AudioProcessChain>0) and (assigned(Module.AudioProcessChains[Channel.AudioProcessChain]) and Module.AudioProcessChains[Channel.AudioProcessChain].Active) then begin
   AudioProcessChain:=Module.AudioProcessChains[Channel.AudioProcessChain];
   Buf:=pointer(@AudioProcessChain.Buffer^[0]);
  end else begin
   AudioProcessChain:=nil;
   Buf:=pointer(@Buffer^[StartPosition*OutputChannels]);
  end;
  Counter:=LengthCounter;
  while Counter>0 do begin
   if (Channel.VolumeRampingCounter<Counter) and (Channel.VolumeRampingCounter>0) then begin
    Remain:=Channel.VolumeRampingCounter;
   end else begin
    Remain:=Counter;
   end;
   Remain:=GetSampleLength(Channel,Remain);
   if Remain>Counter then Remain:=Counter;
   if Remain=0 then begin
    Channel.Active:=false;
    if assigned(AudioProcessChain) then begin
     AudioProcessChain.LastLeft:=AudioProcessChain.LastLeft+Channel.LastLeft;
     AudioProcessChain.LastRight:=AudioProcessChain.LastRight+Channel.LastRight;
    end else begin
     Module.LastLeft:=Module.LastLeft+Channel.LastLeft;
     Module.LastRight:=Module.LastRight+Channel.LastRight;
    end;
    Channel.LastLeft:=0;
    Channel.LastRight:=0;
    break;
   end;
   if Channel.SampleBackwards then begin
    Channel.SampleIncrement:=-Channel.Increment;
   end else begin
    Channel.SampleIncrement:=Channel.Increment;
   end;
   if (Channel.VolumeRampingLeft=0) and (Channel.VolumeRampingRight=0) and (Channel.StepVolumeRampingLeft=0) and (Channel.StepVolumeRampingRight=0) and not Channel.FilterActive then begin
    inc(Channel.SamplePosition,Channel.Increment*Remain);
    Channel.LastLeft:=0;
    Channel.LastRight:=0;
   end else begin
    if assigned(MixSampleProc) then begin
{$ifdef MixerIntegerIncrement}
     ConvertRealToFast;
{$endif}
     LastSampleBuffer:=Buf;
     inc(LastSampleBuffer,(Remain-1)*OutputChannels);
     case OutputChannels of
      1:begin
       Channel.LastLeft:=LastSampleBuffer^;
       MixSampleProc(Buf,Remain,Channel,ResamplingTable);
       Channel.LastLeft:=LastSampleBuffer^-Channel.LastLeft;
      end;
      2:begin
       Channel.LastLeft:=PBeRoXMStereoSample(LastSampleBuffer)^.Left;
       Channel.LastRight:=PBeRoXMStereoSample(LastSampleBuffer)^.Right;
       MixSampleProc(Buf,Remain,Channel,ResamplingTable);
       Channel.LastLeft:=PBeRoXMStereoSample(LastSampleBuffer)^.Left-Channel.LastLeft;
       Channel.LastRight:=PBeRoXMStereoSample(LastSampleBuffer)^.Right-Channel.LastRight;
      end;
     end;
{$ifdef MixerIntegerIncrement}
     ConvertFastToReal;
{$endif}
    end else begin
{$ifdef ChannelOscillators}
     if OscillatorCounter=BeRoXMMixerWithOscillator then begin    
      for SubCounter:=1 to Remain do begin
       with Channel.OscillatorBuffer[Channel.OscillatorPosition] do begin
        Left:=0;
        Right:=0;
       end;
       Channel.OscillatorPosition:=(Channel.OscillatorPosition+1) and BeRoXMOscillatorLengthMask;
      end;
     end;
{$endif}
     inc(Channel.SamplePosition,Channel.Increment*Remain);
     Channel.LastLeft:=0;
     Channel.LastRight:=0;
    end;
   end;
   dec(Counter,Remain);
   if Channel.VolumeRampingCounter>0 then begin
    dec(Channel.VolumeRampingCounter,Remain);
    if Channel.VolumeRampingCounter<=0 then begin
     Channel.VolumeRampingLeft:=Channel.DestVolumeRampingLeft;
     Channel.VolumeRampingRight:=Channel.DestVolumeRampingRight;
     Channel.StepVolumeRampingLeft:=0;
     Channel.StepVolumeRampingRight:=0;
     Channel.VolumeRampingCounter:=0;
     IsVolumeEnvelopeAtEnd:=false;
     if assigned(Channel.VolumeEnvelope.Envelope) and (Channel.EnvelopeVolume=0) then begin
       if Channel.VolumeEnvelope.Envelope.Active and
         (((Channel.VolumeEnvelope.CurrentPoint>Channel.VolumeEnvelope.Envelope.SustainPoint) or
          not Channel.VolumeEnvelope.Envelope.Sustain) and not Channel.VolumeEnvelope.Envelope.Loop) then begin
       IsVolumeEnvelopeAtEnd:=true;
      end;
     end;
     if (Channel.VolumeRampingLeft=0) and (Channel.VolumeRampingRight=0) and
        ((Channel.FadeOut=0) or Channel.IsClickRemovalFadeOutChannel or IsVolumeEnvelopeAtEnd) then begin
      Channel.Active:=false;
      break;
     end;
     ReselectMixSampleProc;
    end;
   end;
  end;
 end;
end;

end.

