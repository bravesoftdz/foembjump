unit DataManager;
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

uses SysUtils,LZBRX,DataUnit;

const DataOK:boolean=false;

type PDataFile=^TDataFile;
     TDataFile=record
      Name:pchar;
      CompressedData:pointer;
      CompressedDataSize:longword;
      CompressedDataCRC32:longword;
      UncompressedDataSize:longword;
     end;

     TDataFiles=array of TDataFile;

var DataFiles:TDataFiles;

procedure InitializeData;

function DataFileGet(FileName:string):PDataFile;
function DataFileExists(FileName:string):boolean;
function DataFileSize(FileName:string):longword;
function DataFileRead(FileName:string;var Data:pointer;var Size:longword):boolean;

implementation

function CRC32(data:pointer;length:longword):longword;
const CRC32Table:array[0..15] of longword=($00000000,$1db71064,$3b6e20c8,$26d930ac,$76dc4190,
                                           $6b6b51f4,$4db26158,$5005713c,$edb88320,$f00f9344,
                                           $d6d6a3e8,$cb61b38c,$9b64c2b0,$86d3d2d4,$a00ae278,
                                           $bdbdf21c);
var buf:pansichar;
    i:longword;
begin
 if length=0 then begin
  result:=0;
 end else begin
  buf:=data;
  result:=$ffffffff;
  for i:=1 to length do begin
   result:=result xor byte(buf^);
   result:=CRC32Table[result and $f] xor (result shr 4);
   result:=CRC32Table[result and $f] xor (result shr 4);
   inc(buf);
  end;
  result:=result xor $ffffffff;
 end;
end;

const Key256:array[0..$ff] of byte=(140,92,54,77,210,218,106,210,116,213,114,136,97,135,140,219,219,17,143,27,230,238,66,52,130,176,15,190,127,43,208,10,
                                    1,15,212,191,245,227,249,160,232,165,247,31,211,39,64,137,48,105,229,54,169,45,220,39,106,193,46,66,33,43,106,220,12,
                                    79,213,149,82,30,147,67,36,34,114,207,254,251,72,117,144,247,2,85,201,187,210,121,166,170,121,140,238,220,22,6,47,5,
                                    240,254,250,17,135,206,71,15,227,154,86,164,65,230,182,41,167,0,236,102,13,247,79,68,193,202,60,154,30,159,141,169,
                                    48,7,141,59,159,16,117,189,132,140,25,17,26,166,62,198,187,103,119,22,154,25,111,96,169,60,62,98,159,187,140,187,104,
                                    13,66,34,23,74,7,72,176,144,0,19,65,62,77,72,36,222,151,73,78,157,131,57,14,238,9,234,72,160,53,223,30,72,148,106,134,
                                    76,117,76,105,97,171,106,245,119,60,26,229,108,28,143,26,110,119,80,77,62,107,69,73,177,137,53,248,81,131,75,87,250,25,
                                    149,90,242,89,96,245,110,45,16,46,23,126,109,70,227,105,62,21,211,72,158,91,50);

procedure DecodeData;
var Key,KeyX,KeyY,KeyZ,KeyW,KeyI,KeyJ:longword;
    i:integer;
begin
 Key:=42;
 KeyX:=380115160;
 KeyY:=123456789;
 KeyZ:=362436069;
 KeyW:=521288629;
 KeyI:=874256314;
 KeyJ:=193489562;
 for i:=0 to DataSize-1 do begin
  KeyX:=(69069*KeyX)+1327217885;
  KeyY:=KeyY xor (KeyY shr 17);
  KeyY:=KeyY xor (KeyY shr 13);
  KeyY:=KeyY xor (KeyY shr 5);
  KeyZ:=((36969*(KeyZ and $ffff))+(KeyZ shr 16)) shl 16;
  KeyW:=((18000*(KeyW and $ffff))+(KeyW shr 16)) and $ffff;
  KeyI:=(KeyI*1664525)+1013904223;
  KeyJ:=(KeyJ*2147001325)+715136305;
  inc(Key,(((KeyZ+KeyW) xor KeyX)+KeyY)-Key256[i and $ff]);
  Key:=(Key shr 13) or (Key shl 19);
  dec(Key,(KeyI xor KeyJ)+Key256[(i*2347) and $ff]);
  Key:=(Key shr 13) or (Key shl 19);
  DataData[i]:=DataData[i] xor (((((Key and $ff)-((Key shr 8) and $ff))+((Key shr 16) and $ff)) xor ((Key shr 24) and $ff)) and $ff);
  inc(Key,DataData[i]*7561);
 end;
end;

procedure InitializeData;
var i,j:integer;
    p:pansichar;
begin
 DataFiles:=nil;
 DecodeData;
 begin
  i:=0;
  p:=@DataData;
  while assigned(p) and (p^<>#0) do begin
   while assigned(p) and (p^<>#0) do begin
    inc(p);
   end;
   inc(p,1+(4*4));
   inc(i);
  end;
  SetLength(DataFiles,i);
 end;
 begin
  i:=0;
  j:=0;
  p:=@DataData;
  while assigned(p) and (p^<>#0) do begin
   DataFiles[i].Name:=p;
   while assigned(p) and (p^<>#0) do begin
    if p^ in ['A'..'Z'] then begin
     inc(byte(p^),ord('a')-ord('A'));
    end;
    inc(p);
   end;
   inc(p);
   DataFiles[i].CompressedData:=@DataData[longword(pointer(p)^)];
   inc(p,4);
   DataFiles[i].CompressedDataSize:=longword(pointer(p)^);
   inc(p,4);
   DataFiles[i].CompressedDataCRC32:=longword(pointer(p)^);
   inc(p,4);
   DataFiles[i].UncompressedDataSize:=longword(pointer(p)^);
   inc(p,4);
   if CRC32(DataFiles[i].CompressedData,DataFiles[i].CompressedDataSize)=DataFiles[i].CompressedDataCRC32 then begin
    inc(j);
   end else begin
    DataFiles[i].Name:=#0;
    DataFiles[i].CompressedData:=nil;
    DataFiles[i].CompressedDataSize:=0;
    DataFiles[i].CompressedDataCRC32:=0;
    DataFiles[i].UncompressedDataSize:=0;
   end;
   inc(i);
  end;
  DataOK:=i=j;
 end;
end;

function DataFileGet(FileName:string):PDataFile;
var i:integer;
begin
 result:=nil;
 FileName:=LowerCase(FileName);
 for i:=0 to length(DataFiles)-1 do begin
  if DataFiles[i].Name=FileName then begin
   result:=@DataFiles[i];
   break;
  end;
 end;
end;

function DataFileExists(FileName:string):boolean;
begin
 result:=assigned(DataFileGet(FileName));
end;

function DataFileSize(FileName:string):longword;
var f:PDataFile;
begin
 f:=DataFileGet(FileName);
 if assigned(f) then begin
  result:=f^.UncompressedDataSize;
 end else begin
  result:=0;
 end;
end;

function DataFileRead(FileName:string;var Data:pointer;var Size:longword):boolean;
var f:PDataFile;
begin
 f:=DataFileGet(FileName);
 if assigned(f) then begin
  Data:=nil;
  Size:=LZBRX.DecompressLZBRX(f.CompressedData,Data,f.CompressedDataSize,nil);
  result:=Size=f.UncompressedDataSize;
  if not result then begin
   Size:=0;
   if assigned(Data) then begin
    FreeMem(Data);
    Data:=nil;
   end;
  end;
 end else begin
  result:=false;
 end;
end;

initialization
 DataFiles:=nil;
finalization
 SetLength(DataFiles,0);
end.
