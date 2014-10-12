unit MathUtils;
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
*){$ifdef fpc}
 {$mode delphi}
 {$h+}
 {$ifdef cpui386}
  {$define cpu386}
 {$endif}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
 {$ifdef FPC_LITTLE_ENDIAN}
  {$define LITTLE_ENDIAN}
 {$else}
  {$ifdef FPC_BIG_ENDIAN}
   {$define BIG_ENDIAN}
  {$endif}
 {$endif}
 {$pic off}
{$endif}

interface

function Floor(FloatValue:single):single; {$ifdef caninline}inline;{$endif}
function Ceil(FloatValue:single):single; {$ifdef caninline}inline;{$endif}
function Modulo(x,y:single):single; {$ifdef cpu386}stdcall; assembler;{$endif}
function ModuloPos(x,y:single):single;

implementation

function Floor(FloatValue:single):single; {$ifdef caninline}inline;{$endif}
begin
 result:=System.int(FloatValue);
 if System.frac(FloatValue)<0 then begin
  result:=result-1;
 end;
end;

function Ceil(FloatValue:single):single; {$ifdef caninline}inline;{$endif}
begin
 result:=System.int(FloatValue);
 if System.frac(FloatValue)>0 then begin
  result:=result+1;
 end;
end;

function Modulo(x,y:single):single;{$ifdef cpu386}stdcall; assembler;
asm
 fld dword ptr y
 fld dword ptr x
 @Repeat:
  fprem
  fstsw ax
  sahf
  jp @Repeat
 fstp st(1)
end;
{$else}
begin
 result:=x-(Floor(x/y)*y);
end;
{$endif}

function ModuloPos(x,y:single):single;
begin
 result:=Modulo(x,y);
 if result<0 then begin
  result:=result+y;
 end;
end;


end.
 