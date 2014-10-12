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
program pckdata;
{$ifdef win32}
 {$apptype console}
{$endif}

uses
  SysUtils,
  Classes,
  BeRoZIP in 'BeRoZIP.pas',
  BeRoZLIB in 'BeRoZLIB.pas',
  BeRoZLIBCore in 'BeRoZLIBCore.pas',
  BeRoStream in 'BeRoStream.pas',
  BeRoStringTree in 'BeRoStringTree.pas',
  BeRoUtils in 'BeRoUtils.pas',
  LZBRX in 'LZBRX.pas';

{$IFDEF WIN32}
CONST DirSplit='\';
{$ELSE}
CONST DirSplit='/';
{$ENDIF}

FUNCTION CorrectFilePath(S:STRING):STRING;
VAR I:INTEGER;
BEGIN
 RESULT:=S;
 I:=LENGTH(RESULT);
 IF I>0 THEN IF RESULT[I]<>DirSplit THEN RESULT:=RESULT+DirSplit;
END;

FUNCTION GetRecursiveFileList(PathEx,Path,Mask:STRING):TStringList;
VAR SR:TSearchRec;
    NewStringList:TStringList;
BEGIN
 RESULT:=TStringList.Create;
 Path:=CorrectFilePath(Path);
 IF FindFirst(PathEx+Path+Mask,faAnyFile AND NOT faDirectory,SR)=0 THEN BEGIN
  REPEAT
   IF (SR.Attr AND faDirectory)=0 THEN BEGIN
    IF (SR.Name<>'.') AND (SR.Name<>'..') THEN BEGIN
     RESULT.Add(Path+SR.Name);
    END;
   END;
  UNTIL FindNext(SR)<>0;
  FindClose(SR);
 END;
 IF FindFirst(PathEx+Path+'*.*',faDirectory,SR)=0 THEN BEGIN
  REPEAT
   IF (SR.Attr AND faDirectory)<>0 THEN BEGIN
    IF (SR.Name<>'.') AND (SR.Name<>'..') THEN BEGIN
     NewStringList:=GetRecursiveFileList(PathEx,CorrectFilePath(Path+SR.Name+DirSplit),Mask);
     RESULT.AddStrings(NewStringList);
     NewStringList.Destroy;
    END;
   END;
  UNTIL FindNext(SR)<>0;
  FindClose(SR);
 END;
END;

procedure PASize(Quelle:TBeRoStream);
VAR Ziel:TBeRoMemoryStream;
    fs:TBeRoFileStream;
    B,C:BYTE;
    S:STRING;
BEGIN
 Ziel:=TBeRoMemoryStream.Create;
 try
  Quelle.Seek(0);
  Ziel.WriteLine('unit dataunit;');
  Ziel.WriteLine('interface');
  Ziel.WriteLine('const DataSize='+inttostr(Quelle.Size)+';');
  Ziel.WriteString('      DataData:ARRAY[0..DataSize-1] of byte=(');
  C:=0;
  WHILE Quelle.Position<Quelle.Size DO BEGIN
   b:=Quelle.ReadByte;
   STR(B,S);
   IF Quelle.Position<>Quelle.Size THEN S:=S+',';
   C:=C+LENGTH(S);
   Ziel.WriteString(S);
   IF C>40 THEN BEGIN
    IF Quelle.Position<>Quelle.Size THEN BEGIN
     Ziel.WriteLine('');
     Ziel.WriteString('                                             ');
    END;
    C:=0;
   END;
  END;
  Ziel.WriteLine(');');
  Ziel.WriteLine('implementation');
  Ziel.WriteLine('end.');
  fs:=TBeRoFileStream.CreateNew('dataunit.pas');
  try
   fs.Assign(Ziel);
  finally
   fs.Destroy;
  end;
 finally
  Ziel.Destroy;
 end;
end;

const sp='..'+DirSplit+'..'+DirSplit+'..'+DirSplit+'data'+DirSplit;

type TFile=record
      Name:ansistring;
      UncompressedData:pointer;
      UncompressedDataSize:longword;
      CompressedData:pointer;
      CompressedDataSize:longword;
      CompressedDataPosition:longword;
      CRC32:longword;
     end;

     TFiles=array of TFile;

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

var ZIP:TBeRoZIP;
    sl:TStringList;
    i,j:integer;
    s:string;
    fs:TBeRoFileStream;
    ms:TBeRoMemoryStream;
    Files:TFiles;
    p:longword;
    Key,KeyX,KeyY,KeyZ,KeyW,KeyI,KeyJ:longword;
    b:byte;
begin
 Files:=nil;
 ZIP:=TBeRoZIP.Create;
 try
  ZIP.Clear;
  sl:=GetRecursiveFileList(sp,'','*.*');
  try
   sl.Sort;
   j:=0;
   for i:=0 to sl.Count-1 do begin
    s:=sl[i];
    if system.pos('.svn',lowercase(s))=0 then begin
     inc(j);
    end;
   end;
   SetLength(Files,j);
   p:=0;
   j:=0;
   for i:=0 to sl.Count-1 do begin
    s:=sl[i];
    if system.pos('.svn',lowercase(s))=0 then begin
     Files[j].Name:=SysUtils.StringReplace(s,'\','/',[rfReplaceAll,rfIgnoreCase]);
     Files[j].Name:=Files[j].Name+#0;
     fs:=TBeRoFileStream.Create(sp+s);
     try
      Files[j].UncompressedDataSize:=fs.Size;
      GetMem(Files[j].UncompressedData,Files[j].UncompressedDataSize);
      fs.Read(Files[j].UncompressedData^,Files[j].UncompressedDataSize);
      Files[j].CompressedDataSize:=CompressLZBRX(Files[j].UncompressedData,Files[j].CompressedData,Files[j].UncompressedDataSize,nil);
      Files[j].CompressedDataPosition:=p;
      inc(p,Files[j].CompressedDataSize);
     finally
      fs.Destroy;
     end;
     inc(j);
    end;
   end;
   for i:=0 to sl.Count-1 do begin
    s:=sl[i];
    if system.pos('.svn',lowercase(s))=0 then begin
     fs:=TBeRoFileStream.Create(sp+s);
     try
      ZIP.SetFile(s,fs,10);
     finally
      fs.Destroy;
     end;
    end;
   end;
  finally
   sl.Destroy;
  end;
  fs:=TBeRoFileStream.CreateNew('data.zip');
  try
   ZIP.WriteArchive(fs);
  finally
   fs.Destroy;
  end;
  ms:=TBeRoMemoryStream.Create;
  try
   p:=0;
   for i:=0 to length(Files)-1 do begin
    inc(p,length(Files[i].Name)+(4*4));
   end;
   inc(p);
   for i:=0 to length(Files)-1 do begin
    inc(Files[i].CompressedDataPosition,p);
   end;
   for i:=0 to length(Files)-1 do begin
    ms.WriteString(Files[i].Name);
    ms.WriteDWord(Files[i].CompressedDataPosition);
    ms.WriteDWord(Files[i].CompressedDataSize);
    Files[i].CRC32:=CRC32(Files[i].CompressedData,Files[i].CompressedDataSize);
    ms.WriteDWord(Files[i].CRC32);
    ms.WriteDWord(Files[i].UncompressedDataSize);
   end;
   ms.WriteByte(0);
   for i:=0 to length(Files)-1 do begin
    ms.Write(Files[i].CompressedData^,Files[i].CompressedDataSize);
   end;
   Key:=42;
   KeyX:=380115160;
   KeyY:=123456789;
   KeyZ:=362436069;
   KeyW:=521288629;
   KeyI:=874256314;
   KeyJ:=193489562;
   for i:=0 to ms.Size-1 do begin
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
    b:=ms.Bytes[i];
    ms.Bytes[i]:=b xor (((((Key and $ff)-((Key shr 8) and $ff))+((Key shr 16) and $ff)) xor ((Key shr 24) and $ff)) and $ff);
    inc(Key,b*7561);
   end;
   fs:=TBeRoFileStream.CreateNew('data.bra');
   try
    fs.Assign(ms);
   finally
    fs.Destroy;
   end;
   PASize(ms);
  finally
   ms.Destroy;
  end;
 finally
  ZIP.Destroy;
 end;
 SetLength(Files,0);
end.

