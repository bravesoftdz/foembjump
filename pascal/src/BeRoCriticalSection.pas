unit BeRoCriticalSection;
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
 {$overflowchecks off}
 {$rangechecks off}
 {$ifdef CPUI386}
  {$define cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef fpc_little_endian}
  {$define littleendian}
 {$else}
  {$ifdef fpc_big_endian}
   {$define bigendian}
  {$endif}
 {$endif}
{$else}
 {$define littleendian}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$optimization on}
{$endif}
{$ifdef fpc}
 {$define caninline}
{$else}
 {$undef caninline}
{$endif}
{$hints off}

interface

{$ifdef fpc}
uses SysUtils;
{$else}
 {$ifdef win32}
uses Windows,Messages;
 {$endif}
{$endif}

type TBeRoCriticalSection=class
      private
       FEntered:boolean;
       FLevel:integer;
       FCriticalSection:TRTLCriticalSection;//{$ifdef android}longint{$else}TRTLCriticalSection{$endif};
      protected
      public
       constructor Create;
       destructor Destroy; override;
       procedure Enter;
       procedure Leave;
       property Entered:boolean read FEntered write FEntered;
     end;

implementation

{$ifdef android}

uses BaseUnix,Unix,UnixType;

type ppthread_mutexattr_t=^pthread_mutexattr_t;
     ppthread_mutex_t=^pthread_mutex_t;

function pthread_mutex_init(__mutex:ppthread_mutex_t; __mutex_attr:ppthread_mutexattr_t):longint;cdecl;external 'c';
function pthread_mutex_destroy(__mutex:ppthread_mutex_t):longint;cdecl;external 'c';
function pthread_mutex_lock(__mutex:ppthread_mutex_t):longint;cdecl;external 'c';
function pthread_mutex_unlock(__mutex:ppthread_mutex_t):longint;cdecl;external 'c';
function pthread_mutexattr_init(__attr:ppthread_mutexattr_t):longint;cdecl;external 'c';
function pthread_mutexattr_destroy(__attr:ppthread_mutexattr_t):longint;cdecl;external 'c';
function pthread_mutexattr_settype(__attr: Ppthread_mutexattr_t; Kind:Integer): Integer; cdecl;external 'c';

procedure InitCriticalSection(var CS);
var MAttr:pthread_mutexattr_t;
    res:longint;
begin
 res:=pthread_mutexattr_init(@MAttr);
 if res=0 then begin
   res:=pthread_mutexattr_settype(@MAttr,longint(_PTHREAD_MUTEX_RECURSIVE));
  if res=0 then begin
   res:=pthread_mutex_init(@CS,@MAttr)
  end else begin
   res:=pthread_mutex_init(@CS,NIL);
  end;
 end else begin
  res:=pthread_mutex_init(@CS,NIL);
 end;
 pthread_mutexattr_destroy(@MAttr);
end;

procedure EnterCriticalSection(var CS);
begin
 pthread_mutex_lock(@CS);
end;

procedure LeaveCriticalSection(var CS);
begin
 pthread_mutex_unlock(@CS);
end;

procedure DoneCriticalSection(var CS);
begin
 while pthread_mutex_unlock(@CS)=0 do begin
 end;
 pthread_mutex_destroy(@CS);
end;       {}

{const FreeSpin=3;
      BusySpin=4;

procedure InitCriticalSection(var CS);
begin
 longint(CS):=FreeSpin;
end;

procedure EnterCriticalSection(var CS);
begin
 while InterlockedCompareExchange(longint(CS),BusySpin,FreeSpin)<>FreeSpin do begin
  sleep(0);
 end;
end;

procedure LeaveCriticalSection(var CS);
begin
 InterlockedExchange(longint(CS),FreeSpin);
end;

procedure DoneCriticalSection(var CS);
begin
 while longint(CS)<>FreeSpin do begin
  sleep(0);
 end;
end;{}
{procedure InitCriticalSection(var CS);
begin
end;

procedure EnterCriticalSection(var CS);
begin
end;

procedure LeaveCriticalSection(var CS);
begin
end;

procedure DoneCriticalSection(var CS);
begin
end;{}
{$endif}

constructor TBeRoCriticalSection.Create;
begin
 inherited Create;
{$ifdef fpc}
 InitCriticalSection(FCriticalSection);
{$else}
 {$ifdef win32}
 InitializeCriticalSection(FCriticalSection);
 {$endif}
{$endif}
 FEntered:=false;
 FLevel:=0;
end;

destructor TBeRoCriticalSection.Destroy;
begin
{$ifdef fpc}
 DoneCriticalSection(FCriticalSection);
{$else}
 {$ifdef win32}
 DeleteCriticalSection(FCriticalSection);
 {$endif}
{$endif}
 inherited Destroy;
end;

procedure TBeRoCriticalSection.Enter;
begin
 EnterCriticalSection(FCriticalSection);
 inc(FLevel);
// Assert(FLevel>0,'Recursive critical section error');
 FEntered:=true;
end;

procedure TBeRoCriticalSection.Leave;
begin
//Assert(FLevel>0,'Recursive critical section error');
 dec(FLevel);
 FEntered:=false;
 LeaveCriticalSection(FCriticalSection);
end;

end.

