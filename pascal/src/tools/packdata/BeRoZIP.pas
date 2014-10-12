UNIT BeRoZIP;
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
 {$WARNINGS OFF}
 {$HINTS OFF}
 {$OVERFLOWCHECKS OFF}
 {$RANGECHECKS OFF}
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
 {$ALIGN ON}
{$ELSE}
 {$DEFINE LITTLE_ENDIAN}
 {$IFNDEF CPU64}
  {$DEFINE CPU32}
 {$ENDIF}
 {$OPTIMIZATION ON}
 {$OVERFLOWCHECKS OFF}
 {$RANGECHECKS OFF}
 {$ALIGN ON}
{$ENDIF}

INTERFACE

USES BeRoStream,BeRoUtils,BeRoStringTree;

CONST BeRoZIPLocalFileHeaderSignature='PK'#3#4;
      BeRoZIPCentralFileHeaderSignature='PK'#1#2;
      BeRoZIPEndCentralFileHeaderSignature='PK'#5#6;

      MaxListSize=2147483647 DIV SIZEOF(POINTER);

TYPE TBeRoZIPHeaderSignature=PACKED ARRAY[1..4] OF CHAR;

     TBeRoZIPLocalFileHeader=PACKED RECORD
      Signature:TBeRoZIPHeaderSignature;
      ExtractVersion:WORD;
      BitFlags:WORD;
      CompressMethod:WORD;
      Time:WORD;
      Date:WORD;
      CRC32:LONGWORD;
      CompressedSize:LONGWORD;
      UncompressedSize:LONGWORD;
      FileNameLength:WORD;
      ExtraFieldLength:WORD;
     END;

     TBeRoZIPCentralFileHeader=PACKED RECORD
      Signature:TBeRoZIPHeaderSignature;
      CreatorVersion:WORD;
      ExtractVersion:WORD;
      BitFlags:WORD;
      CompressMethod:WORD;
      Time:WORD;
      Date:WORD;
      CRC32:LONGWORD;
      CompressedSize:LONGWORD;
      UncompressedSize:LONGWORD;
      FileNameLength:WORD;
      ExtraFieldLength:WORD;
      FileCommentLength:WORD;
      StartDiskNumber:WORD;
      InternalAttrributes:WORD;
      ExternalAttrributes:LONGWORD;
      LocalFileHeaderOffset:LONGWORD;
     END;

     TBeRoZIPEndCentralFileHeader=PACKED RECORD
      Signature:TBeRoZIPHeaderSignature;
      DiskNumber:WORD;
      CentralDirectoryStartDisk:WORD;
      EntriesThisDisk:WORD;
      TotalEntries:WORD;
      CentralDirectorySize:LONGWORD;
      StartDiskOffset:LONGWORD;
      CommentLength:LONGWORD;
     END;

     TBeRoZIPFileName=ARRAY[1..256] OF CHAR;

     TBeRoZIP=CLASS;

     TBeRoZIPFile=CLASS
      PRIVATE
       Host:TBeRoZIP;
       TheFileName:STRING;
       SearchStringTreeFileName:STRING;
       TheStream:TBeRoStream;
       PROCEDURE SetFileName(AFileName:STRING);
      PUBLIC
       LocalFileHeaderPosition:INTEGER;
       LocalFileHeader:TBeRoZIPLocalFileHeader;
       CompressLevel:INTEGER;
       CONSTRUCTOR Create(AHost:TBeRoZIP;AFileName:STRING;AStream:TBeRoStream);
       DESTRUCTOR Destroy; OVERRIDE;
       PROCEDURE Clear;
       PROPERTY FileName:STRInG READ TheFileName WRITE SetFileName;
       PROPERTY Stream:TBeRoStream READ TheStream;
     END;

     PBeRoZIPFileArray=^TBeRoZIPFileArray;
     TBeRoZIPFileArray=ARRAY[0..MaxListSize-1] OF TBeRoZIPFile;

     TBeRoZIPFileList=CLASS
      PRIVATE
       FileList:PBeRoZIPFileArray;
       FileListItemCount,FileListMemorySize:INTEGER;
       Host:TBeRoZIP;
       FUNCTION GetItem(Index:INTEGER):TBeRoZIPFile;
       PROCEDURE SetItem(Index:INTEGER;Value:TBeRoZIPFile);
       FUNCTION GetItemPointer(Index:INTEGER):TBeRoZIPFile;
      PUBLIC
       CONSTRUCTOR Create(AHost:TBeRoZIP);
       DESTRUCTOR Destroy; OVERRIDE;
       PROCEDURE Clear;
       FUNCTION Add(Item:TBeRoZIPFile):INTEGER;
       FUNCTION NewClass(AFileName:STRING;AStream:TBeRoStream):TBeRoZIPFile;
       PROCEDURE Insert(Index:INTEGER;Item:TBeRoZIPFile);
       PROCEDURE Delete(Index:INTEGER);
       FUNCTION Remove(Item:TBeRoZIPFile):INTEGER;
       FUNCTION RemoveClass(Item:TBeRoZIPFile):INTEGER;
       FUNCTION Find(Item:TBeRoZIPFile):INTEGER;
       FUNCTION IndexOf(Item:TBeRoZIPFile):INTEGER;
       PROCEDURE Exchange(Index1,Index2:INTEGER);
       PROCEDURE SetCapacity(NewCapacity:INTEGER);
       PROCEDURE SetCount(NewCount:INTEGER);
       PROPERTY Count:INTEGER READ FileListItemCount;
       PROPERTY Capacity:INTEGER READ FileListMemorySize WRITE SetCapacity;
       PROPERTY Item[Index:INTEGER]:TBeRoZIPFile READ GetItem WRITE SetItem; DEFAULT;
       PROPERTY Items[Index:INTEGER]:TBeRoZIPFile READ GetItem WRITE SetItem;
       PROPERTY PItems[Index:INTEGER]:TBeRoZIPFile READ GetItemPointer;
     END;

     TBeRoZIP=CLASS
      PRIVATE
       StringTree:TBeRoStringTree;
       FileList:TBeRoZIPFileList;
       FUNCTION CompressShrink(InStream,OutStream:TBeRoStream):BOOLEAN;
       FUNCTION Decompress(InStream,OutStream:TBeRoStream;HeaderOffset:LONGWORD):BOOLEAN;
      PUBLIC
       CONSTRUCTOR Create;
       DESTRUCTOR Destroy; OVERRIDE;
       PROCEDURE Clear;
       FUNCTION ReadArchive(Stream:TBeRoStream;OnlyFileName:STRING='';DoUnpack:BOOLEAN=TRUE;DoClear:BOOLEAN=TRUE):BOOLEAN;
       FUNCTION WriteArchive(Stream:TBeRoStream):BOOLEAN;
       FUNCTION GetFile(FileName:STRING;Stream:TBeRoStream):BOOLEAN;
       FUNCTION SetFile(FileName:STRING;Stream:TBeRoStream;CompressLevel:INTEGER=10):BOOLEAN;
       PROPERTY Files:TBeRoZIPFileList READ FileList WRITE FileList;
     END;

IMPLEMENTATION

USES BeRoZLIB;

{$IFNDEF FPC}
TYPE PTRUINT=LONGWORD;
{$ENDIF}

TYPE PBYTE=^BYTE;
     PWORD=^WORD;
     PLONGWORD=^LONGWORD;

CONST CRC32Table:ARRAY[0..255] OF LONGWORD=($00000000,$77073096,$ee0e612c,$990951ba,$076dc419,$706af48f,$e963a535,$9e6495a3,
                                            $0edb8832,$79dcb8a4,$e0d5e91e,$97d2d988,$09b64c2b,$7eb17cbd,$e7b82d07,$90bf1d91,
                                            $1db71064,$6ab020f2,$f3b97148,$84be41de,$1adad47d,$6ddde4eb,$f4d4b551,$83d385c7,
                                            $136c9856,$646ba8c0,$fd62f97a,$8a65c9ec,$14015c4f,$63066cd9,$fa0f3d63,$8d080df5,
                                            $3b6e20c8,$4c69105e,$d56041e4,$a2677172,$3c03e4d1,$4b04d447,$d20d85fd,$a50ab56b,
                                            $35b5a8fa,$42b2986c,$dbbbc9d6,$acbcf940,$32d86ce3,$45df5c75,$dcd60dcf,$abd13d59,
                                            $26d930ac,$51de003a,$c8d75180,$bfd06116,$21b4f4b5,$56b3c423,$cfba9599,$b8bda50f,
                                            $2802b89e,$5f058808,$c60cd9b2,$b10be924,$2f6f7c87,$58684c11,$c1611dab,$b6662d3d,
                                            $76dc4190,$01db7106,$98d220bc,$efd5102a,$71b18589,$06b6b51f,$9fbfe4a5,$e8b8d433,
                                            $7807c9a2,$0f00f934,$9609a88e,$e10e9818,$7f6a0dbb,$086d3d2d,$91646c97,$e6635c01,
                                            $6b6b51f4,$1c6c6162,$856530d8,$f262004e,$6c0695ed,$1b01a57b,$8208f4c1,$f50fc457,
                                            $65b0d9c6,$12b7e950,$8bbeb8ea,$fcb9887c,$62dd1ddf,$15da2d49,$8cd37cf3,$fbd44c65,
                                            $4db26158,$3ab551ce,$a3bc0074,$d4bb30e2,$4adfa541,$3dd895d7,$a4d1c46d,$d3d6f4fb,
                                            $4369e96a,$346ed9fc,$ad678846,$da60b8d0,$44042d73,$33031de5,$aa0a4c5f,$dd0d7cc9,
                                            $5005713c,$270241aa,$be0b1010,$c90c2086,$5768b525,$206f85b3,$b966d409,$ce61e49f,
                                            $5edef90e,$29d9c998,$b0d09822,$c7d7a8b4,$59b33d17,$2eb40d81,$b7bd5c3b,$c0ba6cad,
                                            $edb88320,$9abfb3b6,$03b6e20c,$74b1d29a,$ead54739,$9dd277af,$04db2615,$73dc1683,
                                            $e3630b12,$94643b84,$0d6d6a3e,$7a6a5aa8,$e40ecf0b,$9309ff9d,$0a00ae27,$7d079eb1,
                                            $f00f9344,$8708a3d2,$1e01f268,$6906c2fe,$f762575d,$806567cb,$196c3671,$6e6b06e7,
                                            $fed41b76,$89d32be0,$10da7a5a,$67dd4acc,$f9b9df6f,$8ebeeff9,$17b7be43,$60b08ed5,
                                            $d6d6a3e8,$a1d1937e,$38d8c2c4,$4fdff252,$d1bb67f1,$a6bc5767,$3fb506dd,$48b2364b,
                                            $d80d2bda,$af0a1b4c,$36034af6,$41047a60,$df60efc3,$a867df55,$316e8eef,$4669be79,
                                            $cb61b38c,$bc66831a,$256fd2a0,$5268e236,$cc0c7795,$bb0b4703,$220216b9,$5505262f,
                                            $c5ba3bbe,$b2bd0b28,$2bb45a92,$5cb36a04,$c2d7ffa7,$b5d0cf31,$2cd99e8b,$5bdeae1d,
                                            $9b64c2b0,$ec63f226,$756aa39c,$026d930a,$9c0906a9,$eb0e363f,$72076785,$05005713,
                                            $95bf4a82,$e2b87a14,$7bb12bae,$0cb61b38,$92d28e9b,$e5d5be0d,$7cdcefb7,$0bdbdf21,
                                            $86d3d2d4,$f1d4e242,$68ddb3f8,$1fda836e,$81be16cd,$f6b9265b,$6fb077e1,$18b74777,
                                            $88085ae6,$ff0f6a70,$66063bca,$11010b5c,$8f659eff,$f862ae69,$616bffd3,$166ccf45,
                                            $a00ae278,$d70dd2ee,$4e048354,$3903b3c2,$a7672661,$d06016f7,$4969474d,$3e6e77db,
                                            $aed16a4a,$d9d65adc,$40df0b66,$37d83bf0,$a9bcae53,$debb9ec5,$47b2cf7f,$30b5ffe9,
                                            $bdbdf21c,$cabac28a,$53b39330,$24b4a3a6,$bad03605,$cdd70693,$54de5729,$23d967bf,
                                            $b3667a2e,$c4614ab8,$5d681b02,$2a6f2b94,$b40bbe37,$c30c8ea1,$5a05df1b,$2d02ef8d);

PROCEDURE InitCRC32(VAR CRC32:LONGWORD);
BEGIN
 CRC32:=$FFFFFFFF;
END;

PROCEDURE UpdateCRC32(VAR CRC32:LONGWORD;VAR InputBuffer;InLen:INTEGER);
VAR B:PBYTE;
    Counter:INTEGER;
BEGIN
 B:=@InputBuffer;
 FOR Counter:=1 TO InLen DO BEGIN
  CRC32:=CRC32Table[(CRC32 AND $FF) XOR B^] XOR ((CRC32 SHR 8) AND $00FFFFFF);
  INC(B);
 END;
END;

PROCEDURE UpdateCRC32Stream(VAR CRC32:LONGWORD;InputStream:TBeRoStream;InLen:INTEGER);
VAR Counter:INTEGER;
BEGIN
 FOR Counter:=1 TO InLen DO BEGIN
  CRC32:=CRC32Table[(CRC32 AND $FF) XOR InputStream.ReadByte] XOR ((CRC32 SHR 8) AND $00FFFFFF);
 END;
END;

PROCEDURE DoneCRC32(VAR CRC32:LONGWORD);
BEGIN
 CRC32:=NOT CRC32;
END;

FUNCTION CorrecTBeRoZIPPath(Path:STRING):STRING;
VAR Counter:INTEGER;
BEGIN
 RESULT:=Path;
 FOR Counter:=1 TO LENGTH(RESULT) DO BEGIN
  CASE RESULT[Counter] OF
   '\':RESULT[Counter]:='/';
   ELSE BEGIN
   END;
  END;
 END;
END;

PROCEDURE SwapLocalFileHeader(VAR LocalFileHeader:TBeRoZIPLocalFileHeader);
BEGIN
 SwapLittleEndianData16(LocalFileHeader.ExtractVersion);
 SwapLittleEndianData16(LocalFileHeader.BitFlags);
 SwapLittleEndianData16(LocalFileHeader.CompressMethod);
 SwapLittleEndianData16(LocalFileHeader.Time);
 SwapLittleEndianData16(LocalFileHeader.Date);
 SwapLittleEndianData32(LocalFileHeader.CRC32);
 SwapLittleEndianData32(LocalFileHeader.CompressedSize);
 SwapLittleEndianData32(LocalFileHeader.UncompressedSize);
 SwapLittleEndianData16(LocalFileHeader.FileNameLength);
 SwapLittleEndianData16(LocalFileHeader.ExtraFieldLength);
END;

PROCEDURE SwapCentralFileHeader(VAR CentralFileHeader:TBeRoZIPCentralFileHeader);
BEGIN
 SwapLittleEndianData16(CentralFileHeader.CreatorVersion);
 SwapLittleEndianData16(CentralFileHeader.ExtractVersion);
 SwapLittleEndianData16(CentralFileHeader.BitFlags);
 SwapLittleEndianData16(CentralFileHeader.CompressMethod);
 SwapLittleEndianData16(CentralFileHeader.Time);
 SwapLittleEndianData16(CentralFileHeader.Date);
 SwapLittleEndianData32(CentralFileHeader.CRC32);
 SwapLittleEndianData32(CentralFileHeader.CompressedSize);
 SwapLittleEndianData32(CentralFileHeader.UncompressedSize);
 SwapLittleEndianData16(CentralFileHeader.FileNameLength);
 SwapLittleEndianData16(CentralFileHeader.ExtraFieldLength);
 SwapLittleEndianData16(CentralFileHeader.FileCommentLength);
 SwapLittleEndianData16(CentralFileHeader.StartDiskNumber);
 SwapLittleEndianData16(CentralFileHeader.InternalAttrributes);
 SwapLittleEndianData16(CentralFileHeader.ExternalAttrributes);
 SwapLittleEndianData16(CentralFileHeader.LocalFileHeaderOffset);
END;

CONSTRUCTOR TBeRoZIPFile.Create(AHost:TBeRoZIP;AFileName:STRING;AStream:TBeRoStream);
BEGIN
 INHERITED Create;
 Host:=AHost;
 TheFileName:=CorrecTBeRoZIPPath(AFileName);
 SearchStringTreeFileName:=LOWERCASE(TRIM(TheFileName));
 TheStream:=TBeRoMemoryStream.Create;
 TheStream.Assign(AStream);
 Host.StringTree.Add(SearchStringTreeFileName,PTRUINT(SELF));
 FastFillChar(LocalFileHeader,SIZEOF(TBeRoZIPLocalFileHeader),#0);
 LocalFileHeaderPosition:=0;
 CompressLevel:=10;
END;

DESTRUCTOR TBeRoZIPFile.Destroy;
BEGIN
 Clear;
 TheStream.Destroy;
 INHERITED Destroy;
END;

PROCEDURE TBeRoZIPFile.Clear;
BEGIN
 FastFillChar(LocalFileHeader,SIZEOF(TBeRoZIPLocalFileHeader),#0);
 LocalFileHeaderPosition:=0;
 IF LENGTH(SearchStringTreeFileName)>0 THEN BEGIN
  Host.StringTree.Delete(SearchStringTreeFileName);
 END;
 TheFileName:='';
 SearchStringTreeFileName:='';
 TheStream.Clear;
 CompressLevel:=10;
END;

PROCEDURE TBeRoZIPFile.SetFileName(AFileName:STRING);
BEGIN
 IF LENGTH(SearchStringTreeFileName)>0 THEN BEGIN
  Host.StringTree.Delete(SearchStringTreeFileName);
 END;
 TheFileName:=CorrecTBeRoZIPPath(AFileName);
 SearchStringTreeFileName:=LOWERCASE(TRIM(TheFileName));
 IF LENGTH(SearchStringTreeFileName)>0 THEN BEGIN
  Host.StringTree.Add(SearchStringTreeFileName,PTRUINT(SELF));
 END;
END;

CONSTRUCTOR TBeRoZIPFileList.Create(AHost:TBeRoZIP);
BEGIN
 INHERITED Create;
 Host:=AHost;
 FileListItemCount:=0;
 FileListMemorySize:=0;
 FileList:=NIL;
 Clear;
END;

DESTRUCTOR TBeRoZIPFileList.Destroy;
BEGIN
 Clear;
 INHERITED Destroy;
END;

PROCEDURE TBeRoZIPFileList.Clear;
VAR Counter:INTEGER;
BEGIN
 FOR Counter:=0 TO FileListItemCount-1 DO BEGIN
  IF ASSIGNED(FileList^[Counter]) THEN BEGIN
   TRY
    FileList^[Counter].Destroy;
   EXCEPT
   END;
  END;
 END;
 FileListItemCount:=0;
 FileListMemorySize:=0;
 REALLOCMEM(FileList,0);
END;

PROCEDURE TBeRoZIPFileList.SetCapacity(NewCapacity:INTEGER);
BEGIN
 IF (NewCapacity>=0) AND (NewCapacity<MaxListSize) THEN BEGIN
  REALLOCMEM(FileList,NewCapacity*SIZEOF(TBeRoZIPFile));
  FileListMemorySize:=NewCapacity;
 END;
END;

PROCEDURE TBeRoZIPFileList.SetCount(NewCount:INTEGER);
BEGIN
 IF (NewCount>=0) AND (NewCount<MaxListSize) THEN BEGIN
  IF NewCount<FileListItemCount THEN BEGIN
   FileListItemCount:=NewCount;
  END ELSE IF NewCount>FileListItemCount THEN BEGIN
   IF NewCount>FileListMemorySize THEN BEGIN
    SetCapacity(NewCount);
   END;
   IF FileListItemCount<NewCount THEN BEGIN
    FastFillChar(FileList^[FileListItemCount],(NewCount-FileListItemCount)*SIZEOF(TBeRoZIPFile),#0);
   END;
   FileListItemCount:=NewCount;
  END;
 END;
END;

FUNCTION TBeRoZIPFileList.Add(Item:TBeRoZIPFile):INTEGER;
BEGIN
 IF FileListItemCount=FileListMemorySize THEN BEGIN
  IF FileListMemorySize>64 THEN BEGIN
   INC(FileListMemorySize,FileListMemorySize DIV 4);
  END ELSE IF FileListMemorySize>8 THEN BEGIN
   INC(FileListMemorySize,16);
  END ELSE BEGIN
   INC(FileListMemorySize,4);
  END;
  REALLOCMEM(FileList,FileListMemorySize*SIZEOF(TBeRoZIPFile));
 END;
 FileList^[FileListItemCount]:=Item;
 RESULT:=FileListItemCount;
 INC(FileListItemCount);
END;

FUNCTION TBeRoZIPFileList.NewClass(AFileName:STRING;AStream:TBeRoStream):TBeRoZIPFile;
VAR Item:TBeRoZIPFile;
BEGIN
 Item:=TBeRoZIPFile.Create(Host,AFileName,AStream);
 Add(Item);
 RESULT:=Item;
END;

PROCEDURE TBeRoZIPFileList.Insert(Index:INTEGER;Item:TBeRoZIPFile);
VAR I:INTEGER;
BEGIN
 IF (Index>=0) AND (Index<FileListItemCount) THEN BEGIN
  SetCount(FileListItemCount+1);
  FOR I:=FileListItemCount-1 DOWNTO Index DO FileList^[I+1]:=FileList^[I];
  FileList^[Index]:=Item;
 END ELSE IF Index=FileListItemCount THEN BEGIN
  Add(Item);
 END ELSE IF Index>FileListItemCount THEN BEGIN
  SetCount(Index);
  Add(Item);
 END;
END;

PROCEDURE TBeRoZIPFileList.Delete(Index:INTEGER);
VAR I,J,K:INTEGER;
BEGIN
 IF (Index>=0) AND (Index<FileListItemCount) THEN BEGIN
  K:=FileListItemCount-1;
  J:=Index;
  FOR I:=J TO K-1 DO FileList^[I]:=FileList^[I+1];
  SetCount(K);
 END;
END;

FUNCTION TBeRoZIPFileList.Remove(Item:TBeRoZIPFile):INTEGER;
VAR I,J,K:INTEGER;
BEGIN
 RESULT:=-1;
 K:=FileListItemCount;
 J:=-1;
 FOR I:=0 TO K-1 DO BEGIN
  IF FileList^[I]=Item THEN BEGIN
   J:=I;
   BREAK;
  END;
 END;
 IF J>=0 THEN BEGIN
  DEC(K);
  FOR I:=J TO K-1 DO FileList^[I]:=FileList^[I+1];
  SetCount(K);
  RESULT:=J;
 END;
END;

FUNCTION TBeRoZIPFileList.RemoveClass(Item:TBeRoZIPFile):INTEGER;
VAR I,J,K:INTEGER;
BEGIN
 RESULT:=-1;
 K:=FileListItemCount;
 J:=-1;
 FOR I:=0 TO K-1 DO BEGIN
  IF FileList^[I]=Item THEN BEGIN
   J:=I;
   BREAK;
  END;
 END;
 IF J>=0 THEN BEGIN
  DEC(K);
  FOR I:=J TO K-1 DO FileList^[I]:=FileList^[I+1];
  SetCount(K);
  IF ASSIGNED(Item) THEN BEGIN
   TRY
    Item.Destroy;
   EXCEPT
   END;
  END;
  RESULT:=J;
 END;
END;

FUNCTION TBeRoZIPFileList.Find(Item:TBeRoZIPFile):INTEGER;
VAR I:INTEGER;
BEGIN
 RESULT:=-1;
 FOR I:=0 TO FileListItemCount-1 DO BEGIN
  IF FileList^[I]=Item THEN BEGIN
   RESULT:=I;
   EXIT;
  END;
 END;
END;

FUNCTION TBeRoZIPFileList.IndexOf(Item:TBeRoZIPFile):INTEGER;
VAR I:INTEGER;
BEGIN
 RESULT:=-1;
 FOR I:=0 TO FileListItemCount-1 DO BEGIN
  IF FileList^[I]=Item THEN BEGIN
   RESULT:=I;
   EXIT;
  END;
 END;
END;

PROCEDURE TBeRoZIPFileList.Exchange(Index1,Index2:INTEGER);
VAR TempPointer:TBeRoZIPFile;
BEGIN
 IF (Index1>=0) AND (Index1<FileListItemCount) AND (Index2>=0) AND (Index2<FileListItemCount) THEN BEGIN
  TempPointer:=FileList^[Index1];
  FileList^[Index1]:=FileList^[Index2];
  FileList^[Index2]:=TempPointer;
 END;
END;

FUNCTION TBeRoZIPFileList.GetItem(Index:INTEGER):TBeRoZIPFile;
BEGIN
 IF (Index>=0) AND (Index<FileListItemCount) THEN BEGIN
  RESULT:=FileList^[Index];
 END ELSE BEGIN
  RESULT:=NIL;
 END;
END;

PROCEDURE TBeRoZIPFileList.SetItem(Index:INTEGER;Value:TBeRoZIPFile);
BEGIN
 IF (Index>=0) AND (Index<FileListItemCount) THEN FileList^[Index]:=Value;
END;

FUNCTION TBeRoZIPFileList.GetItemPointer(Index:INTEGER):TBeRoZIPFile;
BEGIN
 RESULT:=NIL;
 IF (Index>=0) AND (Index<FileListItemCount) THEN RESULT:=@FileList^[Index];
END;


CONSTRUCTOR TBeRoZIP.Create;
BEGIN
 INHERITED Create;
 StringTree:=TBeRoStringTree.Create;
 FileList:=TBeRoZIPFileList.Create(SELF);
END;

DESTRUCTOR TBeRoZIP.Destroy;
BEGIN
 FileList.Destroy;
 StringTree.Destroy;
 INHERITED Destroy;
END;

PROCEDURE TBeRoZIP.Clear;
BEGIN
 FileList.Clear;
 StringTree.Clear;
END;

FUNCTION TBeRoZIP.ReadArchive(Stream:TBeRoStream;OnlyFileName:STRING='';DoUnpack:BOOLEAN=TRUE;DoClear:BOOLEAN=TRUE):BOOLEAN;
VAR LocalFileHeader:TBeRoZIPLocalFileHeader;
    HeaderPosition,NextPosition:INTEGER;
    FileName,FileNameEx:STRING;
    FileStream:TBeRoMemoryStream;
    LocalFile:TBeRoZIPFile;
    Link:LONGWORD;
    LinkFile:TBeRoZIPFile ABSOLUTE Link;
BEGIN
 RESULT:=FALSE;
 TRY
  IF DoClear THEN Clear;
  OnlyFileName:=CorrecTBeRoZIPPath(LOWERCASE(TRIM(OnlyFileName)));
  Stream.Seek(0);
  HeaderPosition:=Stream.Position;
  IF Stream.Read(LocalFileHeader,SIZEOF(TBeRoZIPLocalFileHeader))<>SIZEOF(TBeRoZIPLocalFileHeader) THEN EXIT;
  WHILE LocalFileHeader.Signature=BeRoZIPLocalFileHeaderSignature DO BEGIN
   SwapLocalFileHeader(LocalFileHeader);
   SETLENGTH(FileName,LocalFileHeader.FileNameLength);
   IF LocalFileHeader.FileNameLength>0 THEN BEGIN
    IF Stream.Read(FileName[1],LocalFileHeader.FileNameLength)<>LocalFileHeader.FileNameLength THEN EXIT;
   END;
   NextPosition:=Stream.Position+INTEGER(LocalFileHeader.CompressedSize+LocalFileHeader.ExtraFieldLength);

   IF LENGTH(FileName)>0 THEN BEGIN
    IF FileName[LENGTH(FileName)]<>'/' THEN BEGIN
     FileNameEx:=CorrectBeRoZIPPath(LOWERCASE(FileName));
     IF (LENGTH(OnlyFileName)=0) OR (OnlyFileName=FileNameEx) THEN BEGIN
      FileStream:=TBeRoMemoryStream.Create;
      IF DoUnpack THEN Decompress(Stream,FileStream,HeaderPosition);
      IF StringTree.Find(FileNameEx,Link) THEN BEGIN
       LocalFile:=LinkFile;
       LocalFile.TheStream.Assign(FileStream);
       FileStream.Destroy;
      END ELSE BEGIN
       LocalFile:=FileList.NewClass(FileName,FileStream);
      END;
      IF ASSIGNED(LocalFile) THEN BEGIN
       LocalFile.LocalFileHeaderPosition:=HeaderPosition;
       LocalFile.LocalFileHeader:=LocalFileHeader;
      END;
      FileStream.Destroy;
      RESULT:=TRUE;
     END;
    END;
   END;

   Stream.Seek(NextPosition,bsoFromBeginning);
   HeaderPosition:=Stream.Position;
   IF Stream.Read(LocalFileHeader,SIZEOF(TBeRoZIPLocalFileHeader))<>SIZEOF(TBeRoZIPLocalFileHeader) THEN EXIT;
  END;
 EXCEPT
  RESULT:=FALSE;
 END;
END;

FUNCTION TBeRoZIP.WriteArchive(Stream:TBeRoStream):BOOLEAN;
VAR LocalFileHeader:TBeRoZIPLocalFileHeader;
    CentralFileHeader:TBeRoZIPCentralFileHeader;
    EndCentralFileHeader:TBeRoZIPEndCentralFileHeader;
    Counter:INTEGER;
    CompressedStream:TBeRoMemoryStream;
    LocalFile:TBeRoZIPFile;
    Entries:INTEGER;
    StartDiskOffset:INTEGER;
    CentralFileDirectorySize:INTEGER;
BEGIN
 RESULT:=FALSE;
 TRY
  IF FileList.Count>0 THEN BEGIN
   Stream.Clear;
   CompressedStream:=TBeRoMemoryStream.Create;
   FOR Counter:=0 TO FileList.Count-1 DO BEGIN
    LocalFile:=FileList[Counter];
    IF ASSIGNED(LocalFile) THEN BEGIN
     CompressedStream.Clear;
     FastFillChar(LocalFileHeader,SIZEOF(TBeRoZIPLocalFileHeader),#0);
     LocalFileHeader.Signature:=BeRoZIPLocalFileHeaderSignature;
     LocalFileHeader.ExtractVersion:=10;
     LocalFileHeader.BitFlags:=0;
     IF LocalFile.Stream.Size>0 THEN BEGIN
      IF (LocalFile.CompressLevel>1) AND ZCompressStream(LocalFile.Stream,CompressedStream,LocalFile.CompressLevel-1) THEN BEGIN
       LocalFileHeader.BitFlags:=2;
       LocalFileHeader.CompressMethod:=8;
      END ELSE IF (LocalFile.CompressLevel=1) AND CompressShrink(LocalFile.Stream,CompressedStream) THEN BEGIN
       LocalFileHeader.CompressMethod:=1;
      END ELSE BEGIN
       LocalFileHeader.CompressMethod:=0;
       CompressedStream.Assign(LocalFile.Stream);
      END;
      CompressedStream.Seek(0);
     END ELSE BEGIN
      LocalFileHeader.CompressMethod:=0;
     END;
     LocalFileHeader.Time:=LocalFile.LocalFileHeader.Time;
     LocalFileHeader.Date:=LocalFile.LocalFileHeader.Date;
     LocalFileHeader.FileNameLength:=LENGTH(LocalFile.FileName);
     LocalFileHeader.CompressedSize:=CompressedStream.Size;
     IF LocalFileHeader.CompressMethod=8 THEN BEGIN
      IF LocalFileHeader.CompressedSize>=6 THEN BEGIN
       DEC(LocalFileHeader.CompressedSize,6);
      END ELSE BEGIN
       LocalFileHeader.CompressedSize:=0;
      END;
     END;
     InitCRC32(LocalFileHeader.CRC32);
     LocalFile.Stream.Seek(0);
     UpdateCRC32Stream(LocalFileHeader.CRC32,LocalFile.Stream,LocalFile.Stream.Size);
     DoneCRC32(LocalFileHeader.CRC32);
     LocalFileHeader.UncompressedSize:=LocalFile.Stream.Size;
     LocalFile.LocalFileHeaderPosition:=Stream.Position;
     LocalFile.LocalFileHeader:=LocalFileHeader;
     SwapLocalFileHeader(LocalFileHeader);
     IF Stream.Write(LocalFileHeader,SIZEOF(TBeRoZIPLocalFileHeader))<>SIZEOF(TBeRoZIPLocalFileHeader) THEN BEGIN
      CompressedStream.Destroy;
      EXIT;
     END;
     IF LocalFileHeader.FileNameLength>0 THEN BEGIN
      IF Stream.Write(LocalFile.FileName[1],LocalFileHeader.FileNameLength)<>LocalFileHeader.FileNameLength THEN BEGIN
       CompressedStream.Destroy;
       EXIT;
      END;
     END;
     IF LocalFileHeader.CompressMethod=8 THEN BEGIN
      IF LocalFileHeader.CompressedSize>0 THEN BEGIN
       CompressedStream.Seek(2);
       IF Stream.AppendFrom(CompressedStream,LocalFileHeader.CompressedSize)<>INTEGER(LocalFileHeader.CompressedSize) THEN BEGIN
        CompressedStream.Destroy;
        EXIT;
       END;
      END;
     END ELSE BEGIN
      IF Stream.Append(CompressedStream)<>INTEGER(LocalFileHeader.CompressedSize) THEN BEGIN
       CompressedStream.Destroy;
       EXIT;
      END;
     END;
    END;
   END;
   CompressedStream.Destroy;
   Entries:=0;
   StartDiskOffset:=Stream.Position;
   CentralFileDirectorySize:=0;
   FOR Counter:=0 TO FileList.Count-1 DO BEGIN
    LocalFile:=FileList[Counter];
    IF ASSIGNED(LocalFile) THEN BEGIN
     FastFillChar(CentralFileHeader,SIZEOF(TBeRoZIPCentralFileHeader),#0);
     CentralFileHeader.Signature:=BeRoZIPCentralFileHeaderSignature;
     CentralFileHeader.CreatorVersion:=LocalFile.LocalFileHeader.ExtractVersion;
     CentralFileHeader.ExtractVersion:=LocalFile.LocalFileHeader.ExtractVersion;
     CentralFileHeader.BitFlags:=LocalFile.LocalFileHeader.BitFlags;
     CentralFileHeader.CompressMethod:=LocalFile.LocalFileHeader.CompressMethod;
     CentralFileHeader.Time:=LocalFile.LocalFileHeader.Time;
     CentralFileHeader.Date:=LocalFile.LocalFileHeader.Date;
     CentralFileHeader.CRC32:=LocalFile.LocalFileHeader.CRC32;
     CentralFileHeader.CompressedSize:=LocalFile.LocalFileHeader.CompressedSize;
     CentralFileHeader.UncompressedSize:=LocalFile.LocalFileHeader.UncompressedSize;
     CentralFileHeader.FileNameLength:=LocalFile.LocalFileHeader.FileNameLength;
     CentralFileHeader.ExtraFieldLength:=LocalFile.LocalFileHeader.ExtraFieldLength;
     CentralFileHeader.ExternalAttrributes:=$20;
     CentralFileHeader.LocalFileHeaderOffset:=LocalFile.LocalFileHeaderPosition;
     SwapCentralFileHeader(CentralFileHeader);
     IF Stream.Write(CentralFileHeader,SIZEOF(TBeRoZIPCentralFileHeader))<>SIZEOF(TBeRoZIPCentralFileHeader) THEN EXIT;
     IF CentralFileHeader.FileNameLength>0 THEN BEGIN
      IF Stream.Write(LocalFile.FileName[1],CentralFileHeader.FileNameLength)<>CentralFileHeader.FileNameLength THEN BEGIN
       EXIT;
      END;
     END;
     INC(Entries);
     INC(CentralFileDirectorySize,SIZEOF(TBeRoZIPCentralFileHeader)+CentralFileHeader.FileNameLength);
    END;
   END;
   FastFillChar(EndCentralFileHeader,SIZEOF(TBeRoZIPEndCentralFileHeader),#0);
   EndCentralFileHeader.Signature:=BeRoZIPEndCentralFileHeaderSignature;
   EndCentralFileHeader.EntriesThisDisk:=Entries;
   EndCentralFileHeader.TotalEntries:=Entries;
   EndCentralFileHeader.StartDiskOffset:=StartDiskOffset;
   EndCentralFileHeader.CentralDirectorySize:=CentralFileDirectorySize;
   IF Stream.Write(EndCentralFileHeader,SIZEOF(TBeRoZIPEndCentralFileHeader))<>SIZEOF(TBeRoZIPEndCentralFileHeader) THEN EXIT;
   RESULT:=TRUE;
  END;
 EXCEPT
  RESULT:=FALSE;
 END;
END;

FUNCTION TBeRoZIP.GetFile(FileName:STRING;Stream:TBeRoStream):BOOLEAN;
VAR Link:LONGWORD;
    LinkFile:TBeRoZIPFile ABSOLUTE Link;
BEGIN
 RESULT:=StringTree.Find(CorrecTBeRoZIPPath(LOWERCASE(TRIM(FileName))),Link);
 IF RESULT THEN BEGIN
  RESULT:=ASSIGNED(LinkFile);
  IF RESULT THEN BEGIN
   RESULT:=ASSIGNED(LinkFile.Stream);
   IF RESULT THEN BEGIN
    Stream.Assign(LinkFile.Stream);
   END;
  END;
 END;
END;

FUNCTION TBeRoZIP.SetFile(FileName:STRING;Stream:TBeRoStream;CompressLevel:INTEGER=10):BOOLEAN;
VAR Link:LONGWORD;
    LinkFile:TBeRoZIPFile ABSOLUTE Link;
BEGIN
 RESULT:=StringTree.Find(CorrecTBeRoZIPPath(LOWERCASE(TRIM(FileName))),Link);
 IF RESULT THEN BEGIN
  RESULT:=ASSIGNED(LinkFile);
  IF RESULT THEN BEGIN
   RESULT:=ASSIGNED(LinkFile.Stream);
   IF RESULT THEN BEGIN
    LinkFile.Stream.Assign(Stream);
   END;
  END;
 END ELSE BEGIN
  LinkFile:=FileList.NewClass(FileName,Stream);
  RESULT:=ASSIGNED(LinkFile);
  IF RESULT THEN BEGIN
   LinkFile.CompressLevel:=CompressLevel;
   RESULT:=ASSIGNED(LinkFile.Stream);
   IF RESULT THEN BEGIN
    LinkFile.Stream.Assign(Stream);
   END;
  END;
 END;
END;

FUNCTION TBeRoZIP.CompressShrink(InStream,OutStream:TBeRoStream):BOOLEAN;
CONST BufSize=10240;
      MINBITS=9;
      MAXBITS=13;
      TABLESIZE=8191;
      SPECIAL=256;
      INCSIZE=1;
      CLEARCODE=2;
      FIRSTENTRY=257;
      UNUSED=-1;
      STDATTR=$23;
TYPE TCodeTableItem=RECORD
      Child:INTEGER;
      Sibling:INTEGER;
      Suffix:BYTE;
     END;

     PCodeTable=^TCodeTable;
     TCodeTable=ARRAY[0..TABLESIZE] OF TCodeTableItem;

     PFreeList=^TFreeList;
     TFreeList=ARRAY[FIRSTENTRY..TABLESIZE] OF LONGWORD;

     PClearList=^TClearList;
     TClearList=ARRAY[0..1023] Of BYTE;

VAR FirstByte:BOOLEAN;
    TableFull:BOOLEAN;
    SaveByte:BYTE;
    BitsUsed:BYTE;
    CodeSize:BYTE;
    MaxCode:WORD;
    CodeTable:PCodeTable;
    FreeList:PFreeList;
    ClearList:PClearList;
    NextFree:WORD;
    LastCode:INTEGER;

 PROCEDURE Prune(Parent:WORD);
 VAR CurrentChild,NextSibling:INTEGER;
 BEGIN
  CurrentChild:=CodeTable^[Parent].Child;
  WHILE (CurrentChild>=0) AND (CodeTable^[CurrentChild].Child<0) DO BEGIN
   CodeTable^[Parent].Child:=CodeTable^[CurrentChild].Sibling;
   CodeTable^[CurrentChild].Sibling:=-1;
   ClearList^[CurrentChild SHR 3]:=(ClearList^[CurrentChild SHR 3] OR (1 SHL (CurrentChild AND 7)));
   CurrentChild:=CodeTable^[Parent].Child;
  END;
  IF CurrentChild>=0 THEN BEGIN
   Prune(CurrentChild);
   NextSibling:=CodeTable^[CurrentChild].Sibling;
   WHILE NextSibling>=0 DO BEGIN
    IF CodeTable^[NextSibling].Child<0 THEN BEGIN
     CodeTable^[CurrentChild].Sibling:=CodeTable^[NextSibling].Sibling;
     CodeTable^[NextSibling].Sibling:=-1;
     ClearList^[NextSibling SHR 3]:=(ClearList^[NextSibling SHR 3] OR (1 SHL (NextSibling AND 7)));
     NextSibling:=CodeTable^[CurrentChild].Sibling;
    END ELSE BEGIN
     CurrentChild:=NextSibling;
     Prune(CurrentChild);
     NextSibling:=CodeTable^[CurrentChild].Sibling;
    END;
   END;
  END;
 END;

 PROCEDURE TableClear;
 VAR Node:WORD;
 BEGIN
  FastFillChar(ClearList^,SIZEOF(TClearList),#0);
  FOR Node:=0 TO 255 DO Prune(Node);
  NextFree:=TABLESIZE+1;
  FOR Node:=TABLESIZE DOWNTO FIRSTENTRY DO BEGIN
   IF (ClearList^[Node SHR 3] AND (1 SHL (Node AND 7)))<>0 THEN BEGIN
    DEC(NextFree);
    FreeList^[NextFree]:=Node;
   END;
  END;
  IF NextFree<=TABLESIZE THEN TableFull:=FALSE;
 END;

 PROCEDURE TableAdd(Prefix:WORD;Suffix:BYTE);
 VAR FreeNode:WORD;
 BEGIN
  IF NextFree<=TABLESIZE THEN BEGIN
   FreeNode:=FreeList^[NextFree];
   INC(NextFree);
   CodeTable^[FreeNode].Child:=-1;
   CodeTable^[FreeNode].Sibling:=-1;
   CodeTable^[FreeNode].Suffix:=Suffix;
   IF CodeTable^[Prefix].Child=-1 THEN BEGIN
    CodeTable^[Prefix].Child:=FreeNode;
   END ELSE BEGIN
    Prefix:=CodeTable^[Prefix].Child;
    WHILE CodeTable^[Prefix].Sibling<>-1 DO Prefix:=CodeTable^[Prefix].Sibling;
    CodeTable^[Prefix].Sibling:=FreeNode;
   END;
  END;
  IF NextFree>TABLESIZE THEN TableFull:=TRUE;
 END;

 FUNCTION TableLookup(TargetPrefix:INTEGER;TargetSuffix:BYTE;VAR FoundAt:INTEGER):BOOLEAN;
 VAR TempChild:INTEGER;
 BEGIN
  RESULT:=False;
  FoundAt:=-1;
  IF CodeTable^[TargetPrefix].Child=-1 THEN EXIT;
  TempChild:=CodeTable^[TargetPrefix].Child;
  WHILE TRUE DO BEGIN
   WITH CodeTable^[TempChild] DO BEGIN
    IF Suffix=TargetSuffix THEN BEGIn
     FoundAt:=TempChild;
     RESULT:=TRUE;
     BREAK;
    END;
    IF Sibling=-1 THEN BREAK;
    TempChild:=Sibling;
   END;
  END;
 END;

 PROCEDURE PutByte(Value:BYTE);
 BEGIN
  OutStream.Write(Value,SIZEOF(Value));
 END;

 PROCEDURE PutCode(Code:SMALLINT);
 VAR Mask:WORD;
     Agent,LocalSaveByte,LocalBitsUsed,LocalCodeSize:BYTE;
 BEGIN
  LocalSaveByte:=SaveByte;
  LocalBitsUsed:=BitsUsed;
  LocalCodeSize:=CodeSize;
  IF Code=-1 THEN BEGIN
   IF LocalBitsUsed<>0 THEN BEGIN
    PutByte(LocalSaveByte);
   END;
  END ELSE BEGIN
   Mask:=$0001;
   REPEAT
    Agent:=0;
    IF (Code AND Mask)<>0 THEN INC(Agent);
    Mask:=Mask SHL 1;
    Agent:=Agent SHL LocalBitsUsed;
    INC(LocalBitsUsed);
    LocalSaveByte:=LocalSaveByte OR Agent;
    IF LocalBitsUsed=8 THEN BEGIN
     PutByte(LocalSaveByte);
     LocalSaveByte:=0;
     LocalBitsUsed:=0;
    END;
    DEC(LocalCodeSize);
   UNTIL LocalCodeSize=0;
   SaveByte:=LocalSaveByte;
   BitsUsed:=LocalBitsUsed;
  END;
 END;

 PROCEDURE ProcessSuffix(Suffix:INTEGER);
 VAR WhereFound:INTEGER;
 BEGIN
  IF FirstByte THEN BEGIN
   SaveByte:=0;
   BitsUsed:=0;
   CodeSize:=MINBITS;
   MaxCode:=(1 SHL CodeSize)-1;
   LastCode:=Suffix;
   FirstByte:=FALSE;
  END ELSE BEGIN
   IF Suffix<>-1 THEN BEGIN
    IF TableFull THEN BEGIN
     PutCode(LastCode);
     PutCode(SPECIAL);
     PutCode(CLEARCODE);
     TableClear;
     TableAdd(LastCode,Suffix);
     LastCode:=Suffix;
    END ELSE BEGIN
     IF TableLookup(LastCode,Suffix,WhereFound) THEN BEGIN
      LastCode:=WhereFound;
     END ELSE BEGIN
      PutCode(LastCode);
      TableAdd(LastCode,Suffix);
      LastCode:=Suffix;
      IF (FreeList^[NextFree]>MaxCode) AND (CodeSize<MaxBits) THEN BEGIN
       PutCode(SPECIAL);
       PutCode(INCSIZE);
       Inc(CodeSize);
       MaxCode:=(1 SHL CodeSize)-1;
      END;
     END;
    END;
   END ELSE BEGIN
    PutCode(LastCode);
    PutCode(-1);
   END;
  END;
 END;

VAR Counter:INTEGER;
BEGIN
 RESULT:=FALSE;
 TRY
  NEW(CodeTable);
  NEW(FreeList);
  NEW(ClearList);
  FOR Counter:=0 TO TABLESIZE DO BEGIN
   WITH CodeTable^[Counter] Do BEgIN
    Child:=-1;
    Sibling:=-1;
    IF Counter<=255 THEN Suffix:=Counter;
   END;
   IF Counter>=FIRSTENTRY THEN FreeList^[Counter]:=Counter;
  END;
  NextFree:=FIRSTENTRY;
  TableFull:=FALSE;
  FirstByte:=TRUE;
  LastCode:=0;
  InStream.Seek(0);
  FOR Counter:=1 TO InStream.Size DO ProcessSuffix(InStream.ReadByte);
  ProcessSuffix(-1);
  DISPOSE(CodeTable);
  DISPOSE(FreeList);
  DISPOSE(ClearList);
  RESULT:=TRUE;
 EXCEPT
 END;
END;

FUNCTION TBeRoZIP.Decompress(InStream,OutStream:TBeRoStream;HeaderOffset:LONGWORD):BOOLEAN;
CONST StatusOk=0;
      StatusCRCErr=-1;
      StatusWriteErr=-2;
      StatusReadErr=-3;
      StatusZipFileErr=-4;
      StatusUserAbort=-5;
      StatusNotSupported=-6;
      StatusEncrypted=-7;
      StatusInUse=-8;
      StatusInternalError=-9;
      StatusNoMoreItems=-10;
      StatusFileError=-11;
      StatusNoTBeRoZIPfile=-12;
      StatusHeaderTooLarge=-13;
      StatusZipFileOpenError=-14;
      StatusSeriousError=-100;
      StatusMissingParameter=-500;

      HuffmanTreeComplete=0;
      HuffmanTreeIncomplete=1;
      HuffmanTreeError=2;
      HuffmanTreeOutOfMemory=3;

      MaxMax=31*1024;
      SlidingDictionaryWindowSize=$8000;
      InBufferSize=1024*4;
      DefaultLiteralBits=9;
      DefaultDistanceBits=6;
      BitLengthCountMax=16;
      OrderOfBitLengthMax=288;
      HuffManTreeBuildMaxValue=16;

      MaxCode=8192;
      MaxStack=8192;
      InitialCodeSize=9;
      FinalCodeSize=13;

      TFileBufferSize=HIGH(INTEGER)-16;
      TFileNameSize=259;

      SupportedMethods=1 OR (1 SHL 1) OR (1 SHL 6) OR (1 SHL 8);

      MaskBits:ARRAY[0..16] OF WORD=($0000,$0001,$0003,$0007,$000F,$001F,$003F,$007F,$00FF,$01FF,$03FF,$07FF,$0FFF,$1FFF,$3FFF,$7FFF,$FFFF);
      Border:ARRAY[0..18] OF BYTE=(16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15);
      CopyLengthLiteralCodes:ARRAY[0..30] OF WORD=(3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,35,43,51,59,67,83,99,115,131,163,195,227,258,0,0);
      ExtraBitsLiteralCodes:ARRAY[0..30] OF WORD=(0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,0,99,99);
      CopyOffsetDistanceCodes:ARRAY[0..29] OF WORD=(1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,257,385,513,769,1025,1537,2049,3073,4097,6145,8193,12289,16385,24577);
      ExtraBitsDistanceCodes:ARRAY[0..29] OF WORD=(0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13);
      CopyLength2:ARRAY[0..63] OF WORD=(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65);
      CopyLength3:ARRAY[0..63] OF WORD=(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66);
      ExtraBitsTable:ARRAY[0..63] OF WORD=(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8);
      CopyOffserDistanceCodes4:ARRAY[0..63] OF WORD =(1,65,129,193,257,321,385,449,513,577,641,705,769,833,897,961,1025,1089,1153,1217,1281,1345,1409,1473,1537,1601,1665,1729,1793,1857,1921,1985,2049,2113,2177,2241,2305,2369,2433,2497,2561,2625,2689,2753,2817,2881,2945,3009,3073,3137,3201,3265,3329,3393,3457,3521,3585,3649,3713,3777,3841,3905,3969,4033);
      CopyOffserDistanceCodes8:ARRAY[0..63] OF WORD=(1,129,257,385,513,641,769,897,1025,1153,1281,1409,1537,1665,1793,1921,2049,2177,2305,2433,2561,2689,2817,2945,3073,3201,3329,3457,3585,3713,3841,3969,4097,4225,4353,4481,4609,4737,4865,4993,5121,5249,5377,5505,5633,5761,5889,6017,6145,6273,6401,6529,6657,6785,6913,7041,7169,7297,7425,7553,7681,7809,7937,8065);

TYPE PUSBList=^TUSBList;
     TUSBList=ARRAY[0..MaxMax] OF WORD;

     PIOBuffer=^TIOBuffer;
     TIOBuffer=ARRAY[0..InBufferSize-1] OF BYTE;

     PPHuffManTree=^PHuffManTree;
     PHuffManTree=^THuffManTree;
     PHuffManTreeList=^THuffManTreeList;
     THuffManTree=RECORD
      ExtraBits,CodeBits:BYTE;
      ByteSize:WORD;
      LinkList:PHuffManTreeList;
     END;
     THuffManTreeList=ARRAY[0..8190] OF THuffManTree;

TYPE PPreviousCodeTrie=^TPreviousCodeTrie;
     TPreviousCodeTrie=ARRAY[257..MaxCode] OF INTEGER;
     PActualCodeTrie=^TActualCodeTrie;
     TActualCodeTrie=ARRAY[257..MaxCode] OF CHAR;
     PStack=^TStack;
     TStack=ARRAY[0..MaxStack] OF CHAR;

VAR Slide:PCHAR;
    InputBuffer:TIOBuffer;
    InputBufferPosition:INTEGER;
    FilePosition:INTEGER;
    SlideWindowPosition:WORD;
    BitBuffer:LONGWORD;
    BitsInBitBuffer:BYTE;
    InDataStream:TBeRoStream;
    OutDataStream:TBeRoStream;
    CompressedSize:INT64;
    ReachedSize:INT64;
    UncompressedSize:INT64;
    CRC32Value:LONGWORD;
    BitsFlagsType:WORD;
    UserAbort,ItIsAtEnd:BOOLEAN;
    PreviousCode:PPreviousCodeTrie;
    ActualCode:PActualCodeTrie;
    Stack:PStack;
    NextFreeCodeInTrie:INTEGER;

 PROCEDURE UpdateCRC(VAR IOBuffer:TIOBuffer;InLen:INTEGER);
 BEGIN
  UpdateCRC32(CRC32Value,IOBuffer,InLen);
 END;

 PROCEDURE Idle;
 BEGIN
 END;

 PROCEDURE ReadBuffer;
 BEGIN
  IF ReachedSize>(CompressedSize+2) THEN BEGIN
   FilePosition:=SIZEOF(TIOBuffer);
   ItIsAtEnd:=TRUE;
  END ELSE BEGIN
   Idle;
   FilePosition:=InDataStream.Read(InputBuffer,SIZEOF(TIOBuffer));
   IF FilePosition<=0 THEN BEGIN
    FilePosition:=SIZEOF(TIOBuffer);
    ItIsAtEnd:=TRUE;
   END;
   INC(ReachedSize,FilePosition);
   DEC(FilePosition);
  END;
  InputBufferPosition:=0;
 END;

 PROCEDURE ReadByte(VAR B:BYTE);
 BEGIN
  IF InputBufferPosition>FilePosition THEN ReadBuffer;
  B:=InputBuffer[InputBufferPosition];
  INC(InputBufferPosition);
 END;

 PROCEDURE NeedBits(Count:BYTE);
 VAR Value:LONGWORD;
 BEGIN
  WHILE BitsInBitBuffer<Count DO BEGIN
   IF InputBufferPosition>FilePosition THEN ReadBuffer;
   Value:=InputBuffer[InputBufferPosition];
   INC(InputBufferPosition);
   BitBuffer:=BitBuffer OR (Value SHL BitsInBitBuffer);
   INC(BitsInBitBuffer,8);
  END;
 END;

 PROCEDURE DumpBits(Count:BYTE);
 BEGIN
  BitBuffer:=BitBuffer SHR Count;
  DEC(BitsInBitBuffer,Count);
 END;

 FUNCTION Flush(Bytes:LONGWORD):BOOLEAN;
 BEGIN
  RESULT:=OutDataStream.Write(Slide[0],Bytes)=INTEGER(Bytes);
  UpdateCRC(TIOBuffer(POINTER(@Slide[0])^),Bytes);
 END;

 PROCEDURE HuffManTreeFree(T:PHuffManTreeList);
 VAR P,Q:PHuffManTreeList;
     Z:INTEGER;
 BEGIN
  P:=T;
  WHILE ASSIGNED(P) DO BEGIN
   DEC(PTRUINT(P),SIZEOF(THuffManTree));
   Q:=P^[0].LinkList;
   Z:=P^[0].ByteSize;
   FREEMEM(P,(Z+1)*SIZEOF(THuffManTree));
   P:=Q;
  END;
 END;

 FUNCTION HuffManTreeBuild(B:PWORD;N:WORD;S:WORD;D,E:PUSBList;T:PPHuffManTree;VAR M:INTEGER):INTEGER;
 TYPE TBitLengthCountTable=ARRAY[0..BitLengthCountMax+1] OF WORD;
 VAR CodeLengthKCount:WORD;
     BitLengthCountTable:TBitLengthCountTable;
     CurrentCodeCounterRepeatsEveryFEntries:WORD;
     MaxCodeLength:INTEGER;
     TableLevel:INTEGER;
     CurrentCodeCounter:WORD;
     Counter:WORD;
     NumberOfBitsInCurrentCode:INTEGER;
     P:PWORD;
     CurrentTable:PHuffManTreeList;
     TableEntry:THuffManTree;
     TableStack:ARRAY[0..BitLengthCountMax] OF PHuffManTreeList;
     ValuesInOrderOfBitsLength:ARRAY[0..OrderOfBitLengthMax] OF WORD;
     BitsBeforeThisTable:INTEGER;
     BitOffsets:ARRAY[0..BitLengthCountMax+1] OF WORD;
     LLevelBitsInTableOfLevel:ARRAY[-1..BitLengthCountMax+1] OF WORD;
     BitOffsetPointer:PWORD;
     NumberOfDummyCodesAdded:INTEGER;
     NumberOfEntriesInCurrentTable:WORD;
     PT:PHuffManTree;
     EOBCodeLength:WORD;
 BEGIN
  IF N>256 THEN BEGIN
   EOBCodeLength:=PWORD(LONGWORD(B)+(256*SIZEOF(WORD)))^;
  END ELSE BEGIN
   EOBCodeLength:=HuffManTreeBuildMaxValue;
  END;
  FastFillChar(BitLengthCountTable,SIZEOF(TBitLengthCountTable),#0);

  P:=B;
  CurrentCodeCounter:=N;
  REPEAT
   IF P^>BitLengthCountMax THEN BEGIN
    T^:=NIL;
    M:=0;
    HuffManTreeBuild:=HuffmanTreeError;
    EXIT;
   END;
   INC(BitLengthCountTable[P^]);
   INC(PTRUINT(P),SIZEOF(WORD));
   DEC(CurrentCodeCounter);
  UNTIL CurrentCodeCounter=0;
  IF BitLengthCountTable[0]=N THEN BEGIN
   T^:=NIL;
   M:=0;
   HuffManTreeBuild:=HuffmanTreeComplete;
   EXIT;
  END;

  Counter:=1;
  WHILE (Counter<=BitLengthCountMax) AND (BitLengthCountTable[Counter]=0) DO INC(Counter);
  NumberOfBitsInCurrentCode:=Counter;
  IF M<Counter THEN M:=Counter;
  CurrentCodeCounter:=BitLengthCountMax;
  WHILE (CurrentCodeCounter>0) AND (BitLengthCountTable[CurrentCodeCounter]=0) DO DEC(CurrentCodeCounter);
  MaxCodeLength:=CurrentCodeCounter;
  IF M>CurrentCodeCounter THEN M:=CurrentCodeCounter;

  NumberOfDummyCodesAdded:=1 SHL Counter;
  WHILE Counter<CurrentCodeCounter DO BEGIN
   DEC(NumberOfDummyCodesAdded,BitLengthCountTable[Counter]);
   IF NumberOfDummyCodesAdded<0 THEN BEGIN
    HuffManTreeBuild:=HuffmanTreeError;
    EXIT;
   END;
   NumberOfDummyCodesAdded:=NumberOfDummyCodesAdded SHL 1;
   INC(Counter);
  END;
  DEC(NumberOfDummyCodesAdded,BitLengthCountTable[CurrentCodeCounter]);
  IF NumberOfDummyCodesAdded<0 THEN BEGIN
   HuffManTreeBuild:=HuffmanTreeError;
   EXIT;
  END;
  INC(BitLengthCountTable[CurrentCodeCounter],NumberOfDummyCodesAdded);

  BitOffsets[1]:=0;
  Counter:=0;
  P:=PWORD(@BitLengthCountTable);
  INC(PTRUINT(P),SIZEOF(WORD));
  BitOffsetPointer:=PWORD(@BitOffsets);
  INC(PTRUINT(BitOffsetPointer),2*SIZEOF(WORD));
  DEC(CurrentCodeCounter);
  WHILE CurrentCodeCounter<>0 DO BEGIN
   INC(Counter,P^);
   BitOffsetPointer^:=Counter;
   INC(PTRUINT(P),SIZEOF(WORD));
   INC(PTRUINT(BitOffsetPointer),SIZEOF(WORD));
   DEC(CurrentCodeCounter);
  END;

  P:=B;
  CurrentCodeCounter:=0;
  REPEAT
   Counter:=P^;
   INC(PTRUINT(P),SIZEOF(WORD));
   IF Counter<>0 THEN BEGIN
    ValuesInOrderOfBitsLength[BitOffsets[Counter]]:=CurrentCodeCounter;
    INC(BitOffsets[Counter]);
   END;
   INC(CurrentCodeCounter);
  UNTIL CurrentCodeCounter>=N;

  BitOffsets[0]:=0;
  CurrentCodeCounter:=0;
  P:=PWORD(@ValuesInOrderOfBitsLength);
  TableLevel:=-1;
  LLevelBitsInTableOfLevel[-1]:=0;
  BitsBeforeThisTable:=0;
  TableStack[0]:=NIL;
  CurrentTable:=NIL;
  NumberOfEntriesInCurrentTable:=0;

  FOR NumberOfBitsInCurrentCode:=NumberOfBitsInCurrentCode TO MaxCodeLength DO BEGIN
   FOR CodeLengthKCount:=BitLengthCountTable[NumberOfBitsInCurrentCode] DOWNTO 1 DO BEGIN
    WHILE NumberOfBitsInCurrentCode>(BitsBeforeThisTable+LLevelBitsInTableOfLevel[TableLevel]) DO BEGIN
     INC(BitsBeforeThisTable,LLevelBitsInTableOfLevel[TableLevel]);
     INC(TableLevel);
     NumberOfEntriesInCurrentTable:=MaxCodeLength-BitsBeforeThisTable;
     IF NumberOfEntriesInCurrentTable>M THEN NumberOfEntriesInCurrentTable:=M;
     Counter:=NumberOfBitsInCurrentCode-BitsBeforeThisTable;
     CurrentCodeCounterRepeatsEveryFEntries:=1 SHL Counter;
     IF CurrentCodeCounterRepeatsEveryFEntries>(CodeLengthKCount+1) THEN BEGIN
      DEC(CurrentCodeCounterRepeatsEveryFEntries,CodeLengthKCount+1);
      BitOffsetPointer:=@BitLengthCountTable[NumberOfBitsInCurrentCode];
      INC(Counter);
      WHILE Counter<NumberOfEntriesInCurrentTable DO BEGIN
        CurrentCodeCounterRepeatsEveryFEntries:=CurrentCodeCounterRepeatsEveryFEntries SHL 1;
        INC(PTRUINT(BitOffsetPointer),SIZEOF(WORD));
        IF CurrentCodeCounterRepeatsEveryFEntries<=BitOffsetPointer^ THEN BEGIN
         BREAK;
        END ELSE BEGIN
         DEC(CurrentCodeCounterRepeatsEveryFEntries,BitOffsetPointer^);
         INC(Counter);
        END;
       END;
      END;
      IF (BitsBeforeThisTable+Counter>EOBCodeLength) AND (BitsBeforeThisTable<EOBCodeLength) THEN Counter:=EOBCodeLength-BitsBeforeThisTable;
      IF BitsBeforeThisTable=0 THEN Counter:=M;
      NumberOfEntriesInCurrentTable:=1 SHL Counter;
      LLevelBitsInTableOfLevel[TableLevel]:=Counter;

      GETMEM(CurrentTable,(NumberOfEntriesInCurrentTable+1)*SIZEOF(THuffManTree));
      IF NOT ASSIGNED(CurrentTable) THEN BEGIN
       IF TableLevel<>0 THEN HuffManTreeFree(TableStack[0]);
       HuffManTreeBuild:=HuffmanTreeOutOfMemory;
       EXIT;
      END;
      FastFillChar(CurrentTable^,( NumberOfEntriesInCurrentTable+1)* SIZEOF(THuffManTree),#0);
      CurrentTable^[0].ByteSize:=NumberOfEntriesInCurrentTable;
      T^:=@CurrentTable^[1];
      T:=PPHuffManTree(@CurrentTable^[0].LinkList);
      T^:=NIL;
      CurrentTable:=PHuffManTreeList(@CurrentTable^[1]);
      TableStack[TableLevel]:=CurrentTable;
      IF TableLevel<>0 THEN BEGIN
       BitOffsets[TableLevel]:=CurrentCodeCounter;
       TableEntry.CodeBits:=LLevelBitsInTableOfLevel[TableLevel-1];
       TableEntry.ExtraBits:=16+Counter;
       TableEntry.LinkList:=CurrentTable;
       Counter:=(CurrentCodeCounter AND ((1 SHL BitsBeforeThisTable)-1)) SHR (BitsBeforeThisTable-LLevelBitsInTableOfLevel[TableLevel-1]);
       PT:=PHuffManTree(LONGWORD(TableStack[TableLevel-1])-SIZEOF(THuffManTree));
       IF Counter>PT^.ByteSize THEN BEGIN
        HuffManTreeFree(TableStack[0]);
        HuffManTreeBuild:=HuffmanTreeError;
        EXIT;
       END;
       PT:=@TableStack[TableLevel-1]^[Counter];
       PT^:=TableEntry;
      END;
     END;

     TableEntry.CodeBits:=WORD(NumberOfBitsInCurrentCode-BitsBeforeThisTable);
     TableEntry.LinkList:=NIL;
     IF LONGINT(P)>=LONGINT(@ValuesInOrderOfBitsLength[N]) THEN BEGIN
      TableEntry.ExtraBits:=99;
     END ELSE IF P^<S THEN BEGIN
      IF P^<256 THEN BEGIN
       TableEntry.ExtraBits:=16;
      END ELSE BEGIN
       TableEntry.ExtraBits:=15;
      END;
      TableEntry.ByteSize:=P^;
      INC(PTRUINT(P),SIZEOF(WORD));
     END ELSE BEGIN
      IF NOT (ASSIGNED(D) AND ASSIGNED(E)) THEN BEGIN
       HuffManTreeFree(TableStack[0]);
       HuffManTreeBuild:=HuffmanTreeError;
       EXIT;
      END;
      TableEntry.ExtraBits:=WORD(E^[P^-S]);
      TableEntry.ByteSize:=D^[P^-S];
      INC(PTRUINT(P),SIZEOF(WORD));
     END;

     CurrentCodeCounterRepeatsEveryFEntries:=1 SHL(NumberOfBitsInCurrentCode-BitsBeforeThisTable);
     Counter:=CurrentCodeCounter SHR BitsBeforeThisTable;
     WHILE Counter<NumberOfEntriesInCurrentTable DO BEGIN
      CurrentTable^[Counter]:=TableEntry;
      INC(Counter,CurrentCodeCounterRepeatsEveryFEntries);
     END;

     Counter:=1 SHL(NumberOfBitsInCurrentCode-1);
     WHILE(CurrentCodeCounter AND Counter)<> 0 DO BEGIN
      CurrentCodeCounter:=CurrentCodeCounter XOR Counter;
      Counter:=Counter SHR 1;
     END;
     CurrentCodeCounter:=CurrentCodeCounter XOR Counter;

     WHILE ((CurrentCodeCounter AND ((1 SHL BitsBeforeThisTable)-1))<>BitOffsets[TableLevel]) DO BEGIN
      DEC(TableLevel);
      DEC(BitsBeforeThisTable,LLevelBitsInTableOfLevel[TableLevel]);
     END;
   END;
  END;
  IF (NumberOfDummyCodesAdded<>0) AND (MaxCodeLength<>1) THEN BEGIN
   RESULT:=HuffmanTreeIncomplete;
  END ELSE BEGIN
   RESULT:=HuffmanTreeComplete;
  END;
 END;

 FUNCTION InflateCodes(LiteralLengthCodeTable,DistanceCodeTable:PHuffManTreeList;LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits:INTEGER):INTEGER;
 VAR N,D,ElementLength:WORD;
     LMask,DMask:WORD;
     T:PHuffManTree;
     TableEntry:BYTE;
 BEGIN
  LMask:=MaskBits[LiteralLengthCodeTableLookupBits];
  DMask:=MaskBits[DistanceCodeTableLookupBits];
  WHILE NOT (UserAbort OR ItIsAtEnd) DO BEGIN
   NeedBits(LiteralLengthCodeTableLookupBits);
   T:=@LiteralLengthCodeTable^[BitBuffer AND LMask];
   TableEntry:=T^.ExtraBits;
   IF TableEntry>16 THEN BEGIN
    REPEAT
     IF TableEntry=99 THEN BEGIN
      RESULT:=StatusZipFileErr;
      EXIT;
     END;
     DumpBits(T^.CodeBits);
     DEC(TableEntry,16);
     NeedBits(TableEntry);
     T:=@T^.LinkList^[BitBuffer AND MaskBits[TableEntry]];
     TableEntry:=T^.ExtraBits;
    UNTIL TableEntry<=16;
   END;
   DumpBits(T^.CodeBits);
   IF TableEntry=16 THEN BEGIN
    Slide[SlideWindowPosition]:=CHAR(T^.ByteSize);
    INC(SlideWindowPosition);
    IF SlideWindowPosition=SlidingDictionaryWindowSize THEN BEGIN
     IF NOT Flush(SlideWindowPosition) THEN BEGIN
      InflateCodes:=StatusWriteErr;
      EXIT;
     END;
     SlideWindowPosition:=0;
    END;
   END ELSE BEGIN
    IF TableEntry=15 THEN BEGIN
     InflateCodes:=StatusOk;
     EXIT;
    END;
    NeedBits(TableEntry);
    N:=T^.ByteSize+(BitBuffer AND MaskBits[TableEntry]);
    DumpBits(TableEntry);
    NeedBits(DistanceCodeTableLookupBits);
    T:=@DistanceCodeTable^[BitBuffer AND DMask];
    TableEntry:=T^.ExtraBits;
    IF TableEntry>16 THEN BEGIN
     REPEAT
      IF TableEntry=99 THEN BEGIN
       InflateCodes:=StatusZipFileErr;
       EXIT;
      END;
      DumpBits(T^.CodeBits);
      DEC(TableEntry,16);
      NeedBits(TableEntry);
      T:=@T^.LinkList^[BitBuffer AND MaskBits[TableEntry]];
      TableEntry:=T^.ExtraBits;
     UNTIL TableEntry<=16;
    END;
    DumpBits(T^.CodeBits);
    NeedBits(TableEntry);
    D:=SlideWindowPosition-T^.ByteSize-WORD(BitBuffer AND MaskBits[TableEntry]);
    DumpBits(TableEntry);
    REPEAT
     D:=D AND (SlidingDictionaryWindowSize-1);
     IF D>SlideWindowPosition THEN BEGIN
      ElementLength:=SlidingDictionaryWindowSize-D;
     END ELSE BEGIN
      ElementLength:=SlidingDictionaryWindowSize-SlideWindowPosition;
     END;
     IF ElementLength>N THEN ElementLength:=N;
     DEC(N,ElementLength);
     IF (SlideWindowPosition-D)>=ElementLength THEN BEGIN
      MOVE(Slide[D],Slide[SlideWindowPosition],ElementLength);
      INC(SlideWindowPosition,ElementLength);
      INC(D,ElementLength);
     END ELSE BEGIN
      REPEAT
       Slide[SlideWindowPosition]:=Slide[D];
       INC(SlideWindowPosition);
       INC(D);
       DEC(ElementLength);
      UNTIL ElementLength=0;
     END;
     IF SlideWindowPosition=SlidingDictionaryWindowSize THEN BEGIN
      IF NOT Flush(SlideWindowPosition) THEN BEGIN
       RESULT:=StatusWriteErr;
       EXIT;
      END;
      SlideWindowPosition:=0;
     END;
    UNTIL N=0;
   END;
  END;
  IF UserAbort THEN BEGIN
   RESULT:=Statususerabort
  END ELSE BEGIN
   RESULT:=StatusreadErr;
  END;
 END;

 FUNCTION InflateStored:INTEGER;
 VAR NumberOfBlockInBlock:WORD;
 BEGIN
  NumberOfBlockInBlock:=BitsInBitBuffer AND 7;
  DumpBits(NumberOfBlockInBlock);

  NeedBits(16);
  NumberOfBlockInBlock:=BitBuffer AND $FFFF;
  DumpBits(16);
  NeedBits(16);
  IF NumberOfBlockInBlock<>((NOT BitBuffer) AND $FFFF) THEN BEGIN
   RESULT:=StatuszipFileErr;
   EXIT;
  END;
  DumpBits(16);
  WHILE (NumberOfBlockInBlock>0) AND NOT (UserAbort OR ItIsAtEnd) DO BEGIN
   DEC(NumberOfBlockInBlock);
   NeedBits(8);
   Slide[SlideWindowPosition]:=CHAR(BitBuffer);
   INC(SlideWindowPosition);
   IF SlideWindowPosition=SlidingDictionaryWindowSize THEN BEGIN
    IF NOT Flush(SlideWindowPosition)THEN BEGIN
     RESULT:=StatusWriteErr;
     EXIT;
    END;
    SlideWindowPosition:=0;
   END;
   DumpBits(8);
  END;
  IF UserAbort THEN BEGIN
   RESULT:=StatusUserAbort;
  END ELSE IF ItIsAtEnd THEN BEGIN
   RESULT:=StatusreadErr;
  END ELSE BEGIN
   RESULT:=StatusOk;
  END;
 END;

 FUNCTION InflateFixed:INTEGER;
 VAR Counter,Value:INTEGER;
     LiteralLengthCodeTable,DistanceCodeTable:PHuffManTreeList;
     LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits:INTEGER;
     LengthList:ARRAY[0..287] OF WORD;
 BEGIN
  FOR Counter:=0 TO 143 DO LengthList[Counter]:=8;
  FOR Counter:=144 TO 255 DO LengthList[Counter]:=9;
  FOR Counter:=256 TO 279 DO LengthList[Counter]:=7;
  FOR Counter:=280 TO 287 DO LengthList[Counter]:=8;
  LiteralLengthCodeTableLookupBits:=7;
  Value:=HuffManTreeBuild(PWORD(@LengthList),288,257,PUSBList(@CopyLengthLiteralCodes),PUSBList(@ExtraBitsLiteralCodes),PPHuffManTree(@LiteralLengthCodeTable),LiteralLengthCodeTableLookupBits); {@@}
  IF Value<>HuffmanTreeComplete THEN BEGIN
   RESULT:=Value;
   EXIT;
  END;
  FOR Counter:=0 TO 29 DO LengthList[Counter]:=5;
  DistanceCodeTableLookupBits:=5;
  IF HuffManTreeBuild(PWORD(@LengthList),30,0,PUSBList(@CopyOffsetDistanceCodes),PUSBList(@ExtraBitsDistanceCodes),PPHuffManTree(@DistanceCodeTable),DistanceCodeTableLookupBits)>HuffmanTreeIncomplete THEN BEGIN
   HuffManTreeFree(LiteralLengthCodeTable);
   RESULT:=StatusZipFileErr;
   EXIT;
  END;
  RESULT:=InflateCodes(LiteralLengthCodeTable,DistanceCodeTable,LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits);
  HuffManTreeFree(LiteralLengthCodeTable);
  HuffManTreeFree(DistanceCodeTable);
 END;

 FUNCTION InflateDynamic:INTEGER;
 VAR I:INTEGER;
     J:WORD;
     LastLength:WORD;
     BitLengthTableMask:WORD;
     NumberOfLengthsToGet:WORD;
     LiteralLengthCodeTable,
     DistanceCodeTable:PHuffManTreeList;
     LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits:INTEGER;
     NumberOfBitLengthCodes,NumberOfLiteralLengthCodes,NumberOfDistanceCodes:WORD;
     LiteralLengthDistanceCodeLengths:ARRAY[0..288+32-1] OF WORD;
 BEGIN
  NeedBits(5);
  NumberOfLiteralLengthCodes:=257+WORD(BitBuffer) AND $1F;
  DumpBits(5);
  NeedBits(5);
  NumberOfDistanceCodes:=1+WORD(BitBuffer) AND $1F;
  DumpBits(5);
  NeedBits(4);
  NumberOfBitLengthCodes:=4+WORD(BitBuffer) AND $F;
  DumpBits(4);
  IF (NumberOfLiteralLengthCodes>288) OR (NumberOfDistanceCodes>32) THEN BEGIN
   RESULT:=1;
   EXIT;
  END;

  FastFillChar(LiteralLengthDistanceCodeLengths,SIZEOF(LiteralLengthDistanceCodeLengths),#0);
  FOR J:=0 TO NumberOfBitLengthCodes-1 DO BEGIN
   NeedBits(3);
   LiteralLengthDistanceCodeLengths[Border[J]]:=BitBuffer AND 7;
   DumpBits(3);
  END;
  FOR J:=NumberOfBitLengthCodes TO 18 DO LiteralLengthDistanceCodeLengths[Border[J]]:=0;

  LiteralLengthCodeTableLookupBits:=7;
  I:=HuffManTreeBuild(PWORD(@LiteralLengthDistanceCodeLengths),19,19,NIL,NIL,PPHuffManTree(@LiteralLengthCodeTable),LiteralLengthCodeTableLookupBits); {@@}
  IF I<>HuffmanTreeComplete THEN BEGIN
   IF I=HuffmanTreeIncomplete THEN HuffManTreeFree(LiteralLengthCodeTable);
   RESULT:=StatusZipFileErr;
   EXIT;
  END;

  NumberOfLengthsToGet:=NumberOfLiteralLengthCodes+NumberOfDistanceCodes;
  BitLengthTableMask:=MaskBits[LiteralLengthCodeTableLookupBits];
  I:=0;
  LastLength:=0;
  WHILE WORD(I)<NumberOfLengthsToGet DO BEGIN
   NeedBits(LiteralLengthCodeTableLookupBits);
   DistanceCodeTable:=PHuffManTreeList(@LiteralLengthCodeTable^[BitBuffer AND BitLengthTableMask]);
   J:=PHuffManTree(DistanceCodeTable)^.CodeBits;
   DumpBits(J);
   J:=PHuffManTree(DistanceCodeTable)^.ByteSize;
   IF J<16 THEN BEGIN
    LastLength:=J;
    LiteralLengthDistanceCodeLengths[I]:=LastLength;
    INC(I)
   END ELSE IF J=16 THEN BEGIN
    NeedBits(2);
    J:=3+(BitBuffer AND 3);
    DumpBits(2);
    IF (I+J)>NumberOfLengthsToGet THEN BEGIN
     RESULT:=1;
     EXIT;
    END;
    WHILE J>0 DO BEGIN
     LiteralLengthDistanceCodeLengths[I]:=LastLength;
     DEC(J);
     INC(I);
    END;
   END ELSE IF J=17 THEN BEGIN
    NeedBits(3);
    J:=3+(BitBuffer AND 7);
    DumpBits(3);
    IF (I+J)>NumberOfLengthsToGet THEN BEGIN
     RESULT:=1;
     EXIT;
    END;
    WHILE J>0 DO BEGIN
     LiteralLengthDistanceCodeLengths[I]:=0;
     INC(I);
     DEC(J);
    END;
    LastLength:=0;
   END ELSE BEGIN
    NeedBits(7);
    J:=11+(BitBuffer AND $7F);
    DumpBits(7);
    IF (I+J)>NumberOfLengthsToGet THEN BEGIN
     RESULt:=StatuszipfileErr;
     EXIT;
    END;
    WHILE J>0 DO BEGIN
     LiteralLengthDistanceCodeLengths[I]:=0;
     DEC(J);
     INC(I);
    END;
    LastLength:=0;
   END;
  END;
  HuffManTreeFree(LiteralLengthCodeTable);

  LiteralLengthCodeTableLookupBits:=DefaultLiteralBits;
  I:=HuffManTreeBuild(PWORD(@LiteralLengthDistanceCodeLengths),NumberOfLiteralLengthCodes,257,PUSBList(@CopyLengthLiteralCodes),PUSBList(@ExtraBitsLiteralCodes),PPHuffManTree(@LiteralLengthCodeTable),LiteralLengthCodeTableLookupBits);
  IF I<>HuffmanTreeComplete THEN BEGIN
   IF I=HuffmanTreeIncomplete THEN HuffManTreeFree(LiteralLengthCodeTable);
   RESULT:=StatusZipFileErr;
   EXIT;
  END;
  DistanceCodeTableLookupBits:=DefaultDistanceBits;
  I:=HuffManTreeBuild(PWORD(@LiteralLengthDistanceCodeLengths[NumberOfLiteralLengthCodes]),NumberOfDistanceCodes,0,PUSBList(@CopyOffsetDistanceCodes),PUSBList(@ExtraBitsDistanceCodes),PPHuffManTree(@DistanceCodeTable),DistanceCodeTableLookupBits);
  IF I>HuffmanTreeIncomplete THEN BEGIN
   IF I=HuffmanTreeIncomplete THEN HuffManTreeFree(DistanceCodeTable);
   HuffManTreeFree(LiteralLengthCodeTable);
   RESULT:=StatusZipFileErr;
   EXIT;
  END;
  InflateDynamic:=InflateCodes(LiteralLengthCodeTable,DistanceCodeTable,LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits);
  HuffManTreeFree(LiteralLengthCodeTable);
  HuffManTreeFree(DistanceCodeTable);
 END;

 FUNCTION InflateBlock(VAR E:INTEGER):INTEGER;
 VAR T:WORD;
 BEGIN
  NeedBits(1);
  E:=BitBuffer AND 1;
  DumpBits(1);

  NeedBits(2);
  T:=BitBuffer AND 3;
  DumpBits(2);

  CASE T OF
   0:RESULT:=InflateStored;
   1:RESULT:=InflateFixed;
   2:RESULT:=InflateDynamic;
   ELSE RESULT:=StatusZipFileErr;
  END;
 END;

 FUNCTION Inflate:INTEGER;
 VAR LastBlockFlag:INTEGER;
 BEGIN
  InputBufferPosition:=0;
  FilePosition:=-1;
  SlideWindowPosition:=0;
  BitsInBitBuffer:=0;
  BitBuffer:=0;
  REPEAT
   RESULT:=InflateBlock(LastBlockFlag);
   IF RESULT<>0 THEN EXIT;
  UNTIL LastBlockFlag<>0;
  IF NOT Flush(SlideWindowPosition) THEN BEGIN
   RESULT:=StatusWriteErr;
  END ELSE BEGIN
   RESULT:=StatusOk;
  END;
 END;

 FUNCTION CopyStored:INTEGER;
 VAR DoReadInBytes,ReadInBytes:INTEGER;
 BEGIN
  WHILE (ReachedSize<CompressedSize) AND NOT UserAbort DO BEGIN
   DoReadInBytes:=CompressedSize-ReachedSize;
   IF DoReadInBytes>SlidingDictionaryWindowSize THEN DoReadInBytes:=SlidingDictionaryWindowSize;
   ReadInBytes:=InDataStream.read(Slide[0],DoReadInBytes);
   IF ReadInBytes<>DoReadInBytes THEN BEGIN
    RESULT:=StatusReadErr;
    EXIT;
   END;
   IF NOT Flush(ReadInBytes) THEN BEGIN
    RESULT:=StatusWriteErr;
    EXIT;
   END;
   INC(ReachedSize,ReadInBytes);
   Idle;
  END;
  IF UserAbort THEN BEGIN
   RESULT:=StatusUserabort;
  END ELSE BEGIN
   RESULT:=StatusOk;
  END;
 END;

 FUNCTION GetTree(l:PWORD;N:WORD):INTEGER;
 VAR I,K,J,B:WORD;
     ByteBuffer:BYTE;
 BEGIN
  ReadByte(ByteBuffer);
  I:=ByteBuffer;
  INC(I);
  K:=0;
  REPEAT
   ReadByte(ByteBuffer);
   J:=ByteBuffer;
   B:=(J AND $F)+1;
   J:=((J AND $F0) SHR 4)+1;
   IF (K+J)>N THEN BEGIN
    RESULT:=4;
    EXIT;
   END;
   REPEAT
    l^:=B;
    INC(PTRUINT(l),SIZEOF(WORD));
    INC(K);
    DEC(J);
   UNTIL J=0;
   DEC(I);
  UNTIL I=0;
  IF K<>N THEN BEGIN
   RESULT:=4;
  END ELSE BEGIN
   RESULT:=0;
  END;
 END;

 FUNCTION ExplodeLiteral8k(BitLengthCodeTable,LiteralLengthCodeTable,DistanceCodeTable:PHuffManTreeList;BitLengthCodeTableLookupBits,LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits:INTEGER): INTEGER;
 VAR S:LONGINT;
     E:WORD;
     N,D:WORD;
     W:WORD;
     T:PHuffManTree;
     BMask,LMask,DMask:WORD;
     U:WORD;
 BEGIN
  BitBuffer:=0;
  BitsInBitBuffer:=0;
  W:=0;
  U:=1;
  BMask:=MaskBits[BitLengthCodeTableLookupBits];
  LMask:=MaskBits[LiteralLengthCodeTableLookupBits];
  DMask:=MaskBits[DistanceCodeTableLookupBits];
  S:=UncompressedSize;
  WHILE (S>0) AND NOT (UserAbort OR ItIsAtEnd) DO BEGIN
   NeedBits(1);
   IF(BitBuffer AND 1)<>0 THEN BEGIN
    DumpBits(1);
    DEC(S);
    NeedBits(BitLengthCodeTableLookupBits);
    T:=@BitLengthCodeTable^[BMask AND NOT BitBuffer];
    E:=T^.ExtraBits;
    IF E>16 THEN BEGIN
     REPEAT
      IF E=99 THEN BEGIN
       RESULT:=StatusZipFileErr;
       EXIT;
      END;
      DumpBits(T^.CodeBits);
      DEC(E,16);
      NeedBits(E);
      T:=@T^.LinkList^[MaskBits[E] AND NOT BitBuffer];
      E:=T^.ExtraBits;
     UNTIL E<=16;
    END;
    DumpBits(T^.CodeBits);
    Slide[W]:=CHAR(T^.ByteSize);
    INC(W);
    IF W=SlidingDictionaryWindowSize THEN BEGIN
     IF NOT Flush(W) THEN BEGIN
      RESULT:=StatusWriteErr;
      EXIT;
     END;
     W:=0;
     U:=0;
    END;
   END ELSE BEGIN
    DumpBits(1);
    NeedBits(7);
    D:=BitBuffer AND $7F;
    DumpBits(7);
    NeedBits(DistanceCodeTableLookupBits);
    T:=@DistanceCodeTable^[DMask AND NOT BitBuffer];
    E:=T^.ExtraBits;
    IF E>16 THEN BEGIN
     REPEAT
      IF E=99 THEN BEGIN
       RESULT:=StatusZipFileErr;
       EXIT;
      END;
      DumpBits(T^.CodeBits);
      DEC(E,16);
      NeedBits(E);
      T:=@T^.LinkList^[MaskBits[E] AND NOT BitBuffer];
      E:=T^.Extrabits;
     UNTIL E<=16;
    END;
    DumpBits(T^.CodeBits);
    D:=W-D-T^.ByteSize;
    NeedBits(LiteralLengthCodeTableLookupBits);
    T:=@LiteralLengthCodeTable^[LMask AND NOT BitBuffer];
    E:=T^.ExtraBits;
    IF E>16 THEN BEGIN
     REPEAT
      IF E=99 THEN BEGIN
       RESULT:=StatusZipFileErr;
       EXIT;
      END;
      DumpBits(T^.CodeBits);
      DEC(E,16);
      NeedBits(E);
      T:=@T^.LinkList^[MaskBits[E] AND NOT BitBuffer];
      E:=T^.ExtraBits;
     UNTIL E<=16;
    END;
    DumpBits(T^.CodeBits);
    N:=T^.ByteSize;
    IF E<>0 THEN BEGIN
     NeedBits(8);
     INC(N,BYTE(BitBuffer) AND $FF);
     DumpBits(8);
    END;
    DEC(S,N);
    REPEAT
     D:=D AND (SlidingDictionaryWindowSize-1);
     IF D>W THEN BEGIN
      E:=SlidingDictionaryWindowSize-D;
     END ELSE BEGIN
      E:=SlidingDictionaryWindowSize-W;
     END;
     IF E>N THEN E:=N;
     DEC(N,E);
     IF (U<>0) AND (W<=D) THEN BEGIN
      FastFillChar(Slide[W],E,#0);
      INC(W,E);
      INC(D,E);
     END ELSE IF(W-D>=E)THEN BEGIN
      MOVE(Slide[D],Slide[W],E);
      INC(W,E);
      INC(D,E);
     END ELSE BEGIN
      REPEAT
       Slide[W]:=Slide[D];
       INC(W);
       INC(D);
       DEC(E);
      UNTIL E=0;
     END;
     IF W=SlidingDictionaryWindowSize THEN BEGIN
      IF NOT Flush(W)THEN BEGIN
       RESULT:=StatusWriteErr;
       EXIT;
      END;
      W:=0;
      U:=0;
     END;
    UNTIL N=0;
   END;
  END;
  IF UserAbort THEN BEGIN
   RESULT:=StatusUserAbort;
  END ELSE IF NOT Flush(W) THEN BEGIN
   RESULT:=StatusWriteErr;
  END ELSE IF ItIsAtEnd THEN BEGIN
   RESULT:=StatusReadErr;
  END ELSE BEGIN
   RESULT:=StatusOk;
  END;
 END;

 FUNCTION ExplodeLiteral4k(BitLengthCodeTable,LiteralLengthCodeTable,DistanceCodeTable:PHuffManTreeList;BitLengthCodeTableLookupBits,LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits:INTEGER): INTEGER;
 VAR S:LONGINT;
     E:WORD;
     N,D:WORD;
     W:WORD;
     T:PHuffManTree;
     BMask,LMask,DMask:WORD;
     U:WORD;
 BEGIN
  BitBuffer:=0;
  BitsInBitBuffer:=0;
  W:=0;
  U:=1;
  BMask:=MaskBits[BitLengthCodeTableLookupBits];
  LMask:=MaskBits[LiteralLengthCodeTableLookupBits];
  DMask:=MaskBits[DistanceCodeTableLookupBits];
  S:=UncompressedSize;
  WHILE (S>0) AND NOT (UserAbort OR ItIsAtEnd) DO BEGIN
   NeedBits(1);
   IF (BitBuffer AND 1)<>0 THEN BEGIN
    DumpBits(1);
    DEC(S);
    NeedBits(BitLengthCodeTableLookupBits);
    T:=@BitLengthCodeTable^[BMask AND NOT BitBuffer];
    E:=T^.ExtraBits;
    IF E>16 THEN BEGIN
     REPEAT
      IF E=99 THEN BEGIN
       RESULT:=StatusZipFileErr;
       EXIT;
      END;
      DumpBits(T^.CodeBits);
      DEC(E,16);
      NeedBits(E);
      T:=@T^.LinkList^[MaskBits[E] AND NOT BitBuffer];
      E:=T^.ExtraBits;
     UNTIL E<=16;
    END;
    DumpBits(T^.CodeBits);
    Slide[W]:=CHAR(T^.ByteSize);
    INC(W);
    IF W=SlidingDictionaryWindowSize THEN BEGIN
     IF NOT Flush(W) THEN BEGIN
      RESULT:=StatusWriteErr;
      EXIT;
     END;
     W:=0;
     U:=0;
    END;
   END ELSE BEGIN
    DumpBits(1);
    NeedBits(6);
    D:=BitBuffer AND $3F;
    DumpBits(6);
    NeedBits(DistanceCodeTableLookupBits);
    T:=@DistanceCodeTable^[DMask AND NOT BitBuffer];
    E:=T^.ExtraBits;
    IF E>16 THEN BEGIN
     REPEAT
      IF E=99 THEN BEGIN
       RESULT:=StatusZipFileErr;
       EXIT;
      END;
      DumpBits(T^.CodeBits);
      DEC(E,16);
      NeedBits(E);
      T:=@T^.LinkList^[MaskBits[E] AND NOT BitBuffer];
      E:=T^.ExtraBits;
     UNTIL E<=16;
    END;
    DumpBits(T^.CodeBits);
    D:=W-D-T^.ByteSize;
    NeedBits(LiteralLengthCodeTableLookupBits);
    T:=@LiteralLengthCodeTable^[LMask AND NOT BitBuffer];
    E:=T^.ExtraBits;
    IF E>16 THEN BEGIN
     REPEAT
      IF E=99 THEN BEGIN
       RESULT:=StatusZipFileErr;
       EXIT;
      END;
      DumpBits(T^.CodeBits);
      DEC(E,16);
      NeedBits(E);
      T:=@T^.LinkList^[MaskBits[E] AND NOT BitBuffer];
      E:=T^.ExtraBits;
     UNTIL E<=16;
    END;
    DumpBits(T^.CodeBits);
    N:=T^.ByteSize;
    IF E<>0 THEN BEGIN
     NeedBits(8);
     INC(N,BitBuffer AND $FF);
     DumpBits(8);
    END;
    DEC(S,N);
    REPEAT
     D:=D AND (SlidingDictionaryWindowSize-1);
     IF D>W THEN BEGIN
      E:=SlidingDictionaryWindowSize-D;
     END ELSE BEGIN
      E:=SlidingDictionaryWindowSize-W;
     END;
     IF E>N THEN E:=N;
     DEC(N,E);
     IF (U<>0) AND (W<=D) THEN BEGIN
      FastFillChar(Slide[W],E,#0);
      INC(W,E);
      INC(D,E);
     END ELSE IF (W-D)>=E THEN BEGIN
      MOVE(Slide[D],Slide[W],E);
      INC(W,E);
      INC(D,E);
     END ELSE BEGIN
      REPEAT
       Slide[W]:=Slide[D];
       INC(W);
       INC(D);
       DEC(E);
      UNTIL E=0;
     END;
     IF W=SlidingDictionaryWindowSize THEN BEGIN
      IF NOT Flush(W) THEN BEGIN
       RESULT:=StatusWriteErr;
       EXIT;
      END;
      W:=0;
      U:=0;
     END;
    UNTIL N=0;
   END;
  END;
  IF UserAbort THEN BEGIN
   RESULT:=StatusUserAbort;
  END ELSE IF NOT Flush(W) THEN BEGIN
   RESULT:=StatusWriteErr;
  END ELSE IF ItIsAtEnd THEN BEGIN
   RESULT:=StatusReadErr;
  END ELSE BEGIN
   RESULT:=StatusOk;
  END;
 END;

 FUNCTION ExplodeNoLiteral8k(LiteralLengthCodeTable,DistanceCodeTable:PHuffManTreeList;LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits:INTEGER): INTEGER;
 VAR S:LONGINT;
     E:WORD;
     N,D:WORD;
     W:WORD;
     T:PHuffManTree;
     LMask,DMask:WORD;
     U:WORD;
 BEGIN
  BitBuffer:=0;
  BitsInBitBuffer:=0;
  W:=0;
  U:=1;
  LMask:=MaskBits[LiteralLengthCodeTableLookupBits];
  DMask:=MaskBits[DistanceCodeTableLookupBits];
  S:=UncompressedSize;
  WHILE (S>0) AND NOT (UserAbort OR ItIsAtEnd) DO BEGIN
   NeedBits(1);
   IF(BitBuffer AND 1)<>0 THEN BEGIN
    DumpBits(1);
    DEC(S);
    NeedBits(8);
    Slide[W]:=CHAR(BitBuffer);
    INC(W);
    IF W=SlidingDictionaryWindowSize THEN BEGIN
     IF NOT Flush(W)THEN BEGIN
      RESULT:=StatusWriteErr;
      EXIT;
     END;
     W:=0;
     U:=0;
    END;
    DumpBits(8);
   END ELSE BEGIN
    DumpBits(1);
    NeedBits(7);
    D:=BitBuffer AND $7F;
    DumpBits(7);
    NeedBits(DistanceCodeTableLookupBits);
    T:=@DistanceCodeTable^[DMask AND NOT BitBuffer];
    E:=T^.ExtraBits;
    IF E>16 THEN BEGIN
     REPEAT
      IF E=99 THEN BEGIN
       RESULT:=StatusZipFileErr;
       EXIT;
      END;
      DumpBits(T^.CodeBits);
      DEC(E,16);
      NeedBits(E);
      T:=@T^.LinkList^[MaskBits[E] AND NOT BitBuffer];
      E:=T^.ExtraBits;
     UNTIL E<=16;
    END;
    DumpBits(T^.CodeBits);
    D:=W-D-T^.ByteSize;
    NeedBits(LiteralLengthCodeTableLookupBits);
    T:=@LiteralLengthCodeTable^[LMask AND NOT BitBuffer];
    E:=T^.ExtraBits;
    IF E>16 THEN BEGIN
     REPEAT
      IF E=99 THEN BEGIN
       RESULT:=StatusZipFileErr;
       EXIT;
      END;
      DumpBits(T^.CodeBits);
      DEC(E,16);
      NeedBits(E);
      T:=@T^.LinkList^[MaskBits[E] AND NOT BitBuffer];
      E:=T^.ExtraBits;
     UNTIL E<=16;
    END;
    DumpBits(T^.CodeBits);
    N:=T^.ByteSize;
    IF E<>0 THEN BEGIN
     NeedBits(8);
     INC(N,BitBuffer AND $FF);
     DumpBits(8);
    END;
    DEC(S,N);
    REPEAT
     D:=D AND (SlidingDictionaryWindowSize-1);
     IF D>W THEN BEGIN
      E:=SlidingDictionaryWindowSize-D;
     END ELSE BEGIN
      E:=SlidingDictionaryWindowSize-W;
     END;
     IF E>N THEN E:=N;
     DEC(N,E);
     IF (U<>0) AND (W<=D) THEN BEGIN
      FastFillChar(Slide[W],E,#0);
      INC(W,E);
      INC(D,E);
     END ELSE IF(W-D>=E)THEN BEGIN
      MOVE(Slide[D],Slide[W],E);
      INC(W,E);
      INC(D,E);
     END ELSE BEGIN
      REPEAT
       Slide[W]:=Slide[D];
       INC(W);
       INC(D);
       DEC(E);
      UNTIL E=0;
     END;
     IF W=SlidingDictionaryWindowSize THEN BEGIN
      IF NOT Flush(W)THEN BEGIN
       RESULT:=StatusWriteErr;
       EXIT;
      END;
      W:=0;
      U:=0;
     END;
    UNTIL N=0;
   END;
  END;
  IF UserAbort THEN BEGIN
   RESULT:=StatusUserAbort;
  END ELSE IF NOT Flush(W) THEN BEGIN
   RESULT:=StatusWriteErr;
  END ELSE IF ItIsAtEnd THEN BEGIN
   RESULT:=StatusReadErr;
  END ELSE BEGIN
   RESULT:=StatusOk;
  END;
 END;

 FUNCTION ExplodeNoLiteral4k(LiteralLengthCodeTable,DistanceCodeTable:PHuffManTreeList;LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits:INTEGER): INTEGER;
 VAR S:LONGINT;
     E:WORD;
     N,D:WORD;
     W:WORD;
     T:PHuffManTree;
     LMask,DMask:WORD;
     U:WORD;
 BEGIN
  BitBuffer:=0;
  BitsInBitBuffer:=0;
  W:=0;
  U:=1;
  LMask:=MaskBits[LiteralLengthCodeTableLookupBits];
  DMask:=MaskBits[DistanceCodeTableLookupBits];
  S:=UncompressedSize;
  WHILE (S>0) AND NOT (UserAbort OR ItIsAtEnd) DO BEGIN
   NeedBits(1);
   IF(BitBuffer AND 1)<>0 THEN BEGIN
    DumpBits(1);
    DEC(S);
    NeedBits(8);
    Slide[W]:=CHAR(BitBuffer);
    INC(W);
    IF W=SlidingDictionaryWindowSize THEN BEGIN
     IF NOT Flush(W) THEN BEGIN
      RESULT:=StatusWriteErr;
      EXIT;
     END;
     W:=0;
     U:=0;
    END;
    DumpBits(8);
   END ELSE BEGIN
    DumpBits(1);
    NeedBits(6);
    D:=BitBuffer AND $3F;
    DumpBits(6);
    NeedBits(DistanceCodeTableLookupBits);
    T:=@DistanceCodeTable^[DMask AND NOT BitBuffer];
    E:=T^.ExtraBits;
    IF E>16 THEN REPEAT
     IF E=99 THEN BEGIN
      RESULT:=StatusZipFileErr;
      EXIT;
     END;
     DumpBits(T^.CodeBits);
     DEC(E,16);
     NeedBits(E);
     T:=@T^.LinkList^[MaskBits[E] AND NOT BitBuffer];
     E:=T^.ExtraBits;
    UNTIL E<=16;
    DumpBits(T^.CodeBits);
    D:=W-D-T^.ByteSize;
    NeedBits(LiteralLengthCodeTableLookupBits);
    T:=@LiteralLengthCodeTable^[LMask AND NOT BitBuffer];
    E:=T^.ExtraBits;
    IF E>16 THEN REPEAT
     IF E=99 THEN BEGIN
      RESULT:=StatusZipFileErr;
      EXIT;
     END;
     DumpBits(T^.CodeBits);
     DEC(E,16);
     NeedBits(E);
     T:=@T^.LinkList^[MaskBits[E] AND NOT BitBuffer];
     E:=T^.ExtraBits;
    UNTIL E<=16;
    DumpBits(T^.CodeBits);
    N:=T^.ByteSize;
    IF E<>0 THEN BEGIN
     NeedBits(8);
     INC(N,BitBuffer AND $FF);
     DumpBits(8);
    END;
    DEC(S,N);
    REPEAT
     D:=D AND (SlidingDictionaryWindowSize-1);
     IF D>W THEN BEGIN
      E:=SlidingDictionaryWindowSize-D;
     END ELSE BEGIN
      E:=SlidingDictionaryWindowSize-W;
     END;
     IF E>N THEN E:=N;
     DEC(N,E);
     IF (U<>0) AND (W<=D) THEN BEGIN
      FastFillChar(Slide[W],E,#0);
      INC(W,E);
      INC(D,E);
     END ELSE IF(W-D>=E)THEN BEGIN
      MOVE(Slide[D],Slide[W],E);
      INC(W,E);
      INC(D,E);
     END ELSE BEGIN
      REPEAT
       Slide[W]:=Slide[D];
       INC(W);
       INC(D);
       DEC(E);
      UNTIL E=0;
     END;
     IF W=SlidingDictionaryWindowSize THEN BEGIN
      IF NOT Flush(W) THEN BEGIN
       RESULT:=StatusWriteErr;
       EXIT;
      END;
      W:=0;
      U:=0;
     END;
    UNTIL N=0;
   END;
  END;
  IF UserAbort THEN BEGIN
   RESULT:=StatusUserAbort;
  END ELSE IF NOT Flush(W) THEN BEGIN
   RESULT:=StatusWriteErr;
  END ELSE IF ItIsAtEnd THEN BEGIN
   RESULT:=StatusReadErr;
  END ELSE BEGIN
   RESULT:=StatusOk;
  END;
 END;

 FUNCTION Explode:INTEGER;
 VAR BitLengthCodeTable,LiteralLengthCodeTable,DistanceCodeTable:PHuffManTreeList;
     BitLengthCodeTableLookupBits,LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits:INTEGER;
     TreeTable:ARRAY[0..255] OF WORD;
 BEGIN
  InputBufferPosition:=0;
  FilePosition:=-1;
  LiteralLengthCodeTableLookupBits:=7;
  IF CompressedSize>200000 THEN DistanceCodeTableLookupBits:=8 ELSE DistanceCodeTableLookupBits:=7;
  IF (BitsFlagsType AND 4)<>0 THEN BEGIN
   BitLengthCodeTableLookupBits:=9;
   RESULT:=GetTree(@TreeTable[0],256);
   IF RESULT<>0 THEN BEGIN
    RESULT:=StatusZipFileErr;
    EXIT;
   END;
   RESULT:=HuffManTreeBuild(PWORD(@TreeTable),256,256,NIL,NIL,PPHuffManTree(@BitLengthCodeTable),BitLengthCodeTableLookupBits);
   IF RESULT<>0 THEN BEGIN
    IF RESULT=HuffmanTreeIncomplete THEN HuffManTreeFree(BitLengthCodeTable);
    RESULT:=StatusZipFileErr;
    EXIT;
   END;
   RESULT:=GetTree(@TreeTable[0],64);
   IF RESULT<>0 THEN BEGIN
    HuffManTreeFree(BitLengthCodeTable);
    RESULT:=StatusZipFileErr;
    EXIT;
   END;
   RESULT:=HuffManTreeBuild(PWORD(@TreeTable),64,0,PUSBList(@CopyLength3),PUSBList(@ExtraBitsTable),PPHuffManTree(@LiteralLengthCodeTable),LiteralLengthCodeTableLookupBits);
   IF RESULT<>0 THEN BEGIN
    IF RESULT=HuffmanTreeIncomplete THEN HuffManTreeFree(LiteralLengthCodeTable);
    HuffManTreeFree(BitLengthCodeTable);
    RESULT:=StatusZipFileErr;
    EXIT;
   END;
   RESULT:=GetTree(@TreeTable[0],64);
   IF RESULT<>0 THEN BEGIN
    HuffManTreeFree(BitLengthCodeTable);
    HuffManTreeFree(LiteralLengthCodeTable);
    RESULT:=StatusZipFileErr;
    EXIT;
   END;
   IF (BitsFlagsType AND 2)<>0 THEN BEGIN
    RESULT:=HuffManTreeBuild(PWORD(@TreeTable),64,0,PUSBList(@CopyOffserDistanceCodes8),PUSBList(@ExtraBitsTable),PPHuffManTree(@DistanceCodeTable),DistanceCodeTableLookupBits);
    IF RESULT<>0 THEN BEGIN
     IF RESULT=HuffmanTreeIncomplete THEN HuffManTreeFree(DistanceCodeTable);
     HuffManTreeFree(BitLengthCodeTable);
     HuffManTreeFree(LiteralLengthCodeTable);
     RESULT:=StatusZipFileErr;
     EXIT;
    END;
    RESULT:=ExplodeLiteral8k(BitLengthCodeTable,LiteralLengthCodeTable,DistanceCodeTable,BitLengthCodeTableLookupBits,LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits);
   END ELSE BEGIN
    RESULT:=HuffManTreeBuild(PWORD(@TreeTable),64,0,PUSBList(@CopyOffserDistanceCodes4),PUSBList(@ExtraBitsTable),PPHuffManTree(@DistanceCodeTable),DistanceCodeTableLookupBits);
    IF RESULT<>0 THEN BEGIN
     IF RESULT=HuffmanTreeIncomplete THEN HuffManTreeFree(DistanceCodeTable);
     HuffManTreeFree(BitLengthCodeTable);
     HuffManTreeFree(LiteralLengthCodeTable);
     RESULT:=StatusZipFileErr;
     EXIT;
    END;
    RESULT:=ExplodeLiteral4k(BitLengthCodeTable,LiteralLengthCodeTable,DistanceCodeTable,BitLengthCodeTableLookupBits,LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits);
   END;
   HuffManTreeFree(DistanceCodeTable);
   HuffManTreeFree(LiteralLengthCodeTable);
   HuffManTreeFree(BitLengthCodeTable);
  END ELSE BEGIN
   RESULT:=GetTree(@TreeTable[0],64);
   IF RESULT<>0 THEN BEGIN
    RESULT:=StatusZipFileErr;
    EXIT;
   END;
   RESULT:=HuffManTreeBuild(PWORD(@TreeTable),64,0,PUSBList(@CopyLength2),PUSBList(@ExtraBitsTable),PPHuffManTree(@LiteralLengthCodeTable),LiteralLengthCodeTableLookupBits);
   IF RESULT<>0 THEN BEGIN
    IF RESULT=HuffmanTreeIncomplete THEN HuffManTreeFree(LiteralLengthCodeTable);
    RESULT:=StatusZipFileErr;
    EXIT;
   END;
   RESULT:=GetTree(@TreeTable[0],64);
   IF RESULT<>0 THEN BEGIN
    HuffManTreeFree(LiteralLengthCodeTable);
    RESULT:=StatusZipFileErr;
    EXIT;
   END;
   IF (BitsFlagsType AND 2)<>0 THEN BEGIN
    RESULT:=HuffManTreeBuild(PWORD(@TreeTable),64,0,PUSBList(@CopyOffserDistanceCodes8),PUSBList(@ExtraBitsTable),PPHuffManTree(@DistanceCodeTable),DistanceCodeTableLookupBits);
    IF RESULT<>0 THEN BEGIN
     IF RESULT=HuffmanTreeIncomplete THEN HuffManTreeFree(DistanceCodeTable);
     HuffManTreeFree(LiteralLengthCodeTable);
     RESULT:=StatusZipFileErr;
     EXIT;
    END;
    RESULT:=ExplodeNoLiteral8k(LiteralLengthCodeTable,DistanceCodeTable,LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits);
   END ELSE BEGIN
    RESULT:=HuffManTreeBuild(PWORD(@TreeTable),64,0,PUSBList(@CopyOffserDistanceCodes4),PUSBList(@ExtraBitsTable),PPHuffManTree(@DistanceCodeTable),DistanceCodeTableLookupBits);
    IF RESULT<>0 THEN BEGIN
     IF RESULT=HuffmanTreeIncomplete THEN HuffManTreeFree(DistanceCodeTable);
     HuffManTreeFree(LiteralLengthCodeTable);
     RESULT:=StatusZipFileErr;
     EXIT;
    END;
    RESULT:=ExplodeNoLiteral4k(LiteralLengthCodeTable,DistanceCodeTable,LiteralLengthCodeTableLookupBits,DistanceCodeTableLookupBits);
   END;
   HuffManTreeFree(DistanceCodeTable);
   HuffManTreeFree(LiteralLengthCodeTable);
  END;
 END;

 FUNCTION WriteChar(C:CHAR):BOOLEAN;
 BEGIN
  RESULT:=OutDataStream.Write(C,SIZEOF(CHAR))=SIZEOF(CHAR);
  UpdateCRC32(CRC32Value,C,SIZEOF(CHAR));
 END;

 PROCEDURE ClearLeafNodes;
 VAR PreviousCodeValue:INTEGER;
     Index:INTEGER;
     MaxActualCode:INTEGER;
     CurrentPreviousCodeTrie:PPreviousCodeTrie;
 BEGIN
  CurrentPreviousCodeTrie:=PreviousCode;
  MaxActualCode:=NextFreeCodeInTrie-1;         
  FOR Index:=257 TO MaxActualCode DO BEGIN
   CurrentPreviousCodeTrie^[Index]:=CurrentPreviousCodeTrie^[Index] OR $8000;
  END;
  FOR Index:=257 TO MaxActualCode DO BEGIN
   PreviousCodeValue:=CurrentPreviousCodeTrie^[Index] AND NOT $8000;
   IF PreviousCodeValue>256 THEN BEGIN
    CurrentPreviousCodeTrie^[PreviousCodeValue]:=CurrentPreviousCodeTrie^[PreviousCodeValue] AND NOT $8000;
   END;
  END;
  PreviousCodeValue:=-1;
  NextFreeCodeInTrie:=-1;
  FOR Index:=257 TO MaxActualCode DO BEGIN
   IF (CurrentPreviousCodeTrie^[Index] AND $C000)<>0 THEN BEGIN
    IF PreviousCodeValue<>-1 THEN BEGIN
     CurrentPreviousCodeTrie^[PreviousCodeValue]:=-Index;
    END ELSE BEGIN
     NextFreeCodeInTrie:=Index;
    END;
    PreviousCodeValue:=Index;
   END;
  END;
  IF PreviousCodeValue<>-1 THEN CurrentPreviousCodeTrie^[PreviousCodeValue]:=-MaxActualCode-1;
 END;

 FUNCTION Unshrink:INTEGER;
 VAR InputCode:INTEGER;
     LastInputCode:INTEGER;
     LastOutputCode:CHAR;
     ActualCodeSize:BYTE;
     StackPtr:INTEGER;
     NewCode:INTEGER;
     CodeMask:INTEGER;
     Index:INTEGER;
     BitsToRead:INTEGER;
 BEGIN
  IF CompressedSize=MAXLONGINT THEN BEGIN
   RESULT:=StatusNotSupported;
   EXIT;
  END;
  InputBufferPosition:=0;
  FilePosition:=-1;

  SlideWindowPosition:=0;
  BitsInBitBuffer:=0;
  BitBuffer:=0;

  NEW(PreviousCode);
  NEW(ActualCode);
  NEW(Stack);

  FastFillChar(PreviousCode^,SIZEOF(TPreviousCodeTrie),#0);
  FastFillChar(ActualCode^,SIZEOF(TActualCodeTrie),#0);
  FastFillChar(Stack^,SIZEOF(TStack),#0);

  FOR Index:=257 TO MaxCode DO PreviousCode^[Index]:=-(Index+1);
  NextFreeCodeInTrie:=257;
  StackPtr:=MaxStack;
  ActualCodeSize:=InitialCodeSize;
  CodeMask:=MaskBits[ActualCodeSize];

  NeedBits(ActualCodeSize);
  InputCode:=BitBuffer AND CodeMask;
  DumpBits(ActualCodeSize);

  LastInputCode:=InputCode;
  LastOutputCode:=CHAR(InputCode AND $FF);

  IF NOT WriteChar(LastOutputCode) THEN BEGIN
   RESULT:=StatuswriteErr;
   EXIT;
  END;

  BitsToRead:=(8*CompressedSize)-ActualCodeSize;

  WHILE (BitsToRead>=ActualCodeSize) AND NOT UserAbort DO BEGIN
   NeedBits(ActualCodeSize);
   InputCode:=BitBuffer AND CodeMask;
   DumpBits(ActualCodeSize);
   DEC(BitsToRead,ActualCodeSize);
   IF InputCode=256 THEN BEGIN
    NeedBits(ActualCodeSize);
    InputCode:=BitBuffer AND CodeMask;
    DumpBits(ActualCodeSize);
    DEC(BitsToRead,ActualCodeSize);
    CASE InputCode OF
     1:BEGIN
      INC(ActualCodeSize);
      IF ActualCodeSize>FinalCodeSize THEN BEGIN
       RESULT:=StatusZipFileErr;
       EXIT;
      END;
      CodeMask:=MaskBits[ActualCodeSize];
     END;
     2:ClearLeafNodes;
     ELSE BEGIN
      RESULT:=StatusZipFileErr;
      EXIT;
     END;
    END;
   END ELSE BEGIN
    NewCode:=InputCode;
    IF InputCode<256 THEN BEGIN
     LastOutputCode:=CHAR(InputCode AND $FF);
     IF NOT WriteChar(LastOutputCode) THEN BEGIN
      RESULT:=StatusWriteErr;
      EXIT;
     END;
    END ELSE BEGIN
     IF PreviousCode^[InputCode]<0 THEN BEGIN
      Stack^[StackPtr]:=LastOutputCode;
      DEC(StackPtr);
      InputCode:=LastInputCode;
     END;
     WHILE InputCode>256 DO BEGIN
      Stack^[StackPtr]:=ActualCode^[InputCode];
      DEC(StackPtr);
      InputCode:=PreviousCode^[InputCode];
     END;
     LastOutputCode:=CHAR(InputCode AND $FF);
     IF NOT WriteChar(LastOutputCode) THEN BEGIN
      RESULT:=StatusWriteErr;
      EXIT;
     END;
     FOR Index:=StackPtr+1 TO MaxStack DO BEGIN
      IF NOT WriteChar(Stack^[Index]) THEN BEGIN
       RESULT:=StatusWriteErr;
       EXIT;
      END;
     END;
     StackPtr:=MaxStack;
    END;
    InputCode:=NextFreeCodeInTrie;
    IF InputCode<=MaxCode THEN BEGIN
     NextFreeCodeInTrie:=-PreviousCode^[InputCode];
     PreviousCode^[InputCode]:=LastInputCode;
     ActualCode^[InputCode]:=LastOutputCode;
    END;
    LastInputCode:=NewCode;
   END;
  END;
  DISPOSE(PreviousCode);
  DISPOSE(ActualCode);
  DISPOSE(Stack);
  IF UserAbort THEN BEGIN
   RESULT:=StatusUserAbort;
  END ELSE BEGIN
   RESULT:=StatusOk;
  END;
 END;

 FUNCTION DoDecompress(InStream:TBeRoStream;OutStream:TBeRoStream;Offset:INTEGER):INTEGER;
 VAR LocalFileHeader:TBeRoZIPLocalFileHeader;
     OriginalCRC:LONGWORD;
     CompressMethod:INTEGER;
 BEGIN
  GETMEM(Slide,SlidingDictionaryWindowSize);
  FastFillChar(Slide^,SlidingDictionaryWindowSize,#0);

  InDataStream:=InStream;
  IF InDataStream.Size=0 THEN BEGIN
   FREEMEM(Slide);
   RESULT:=StatusZipFileOpenError;
   EXIT;
  END;

  IF InDataStream.Seek(Offset)<>Offset THEN BEGIN
   FREEMEM(Slide);
   RESULT:=StatusReadErr;
   EXIT;
  END;

  IF InDataStream.Read(LocalFileHeader,SIZEOF(TBeRoZIPLocalFileHeader))<>SIZEOF(TBeRoZIPLocalFileHeader) THEN BEGIN
   FREEMEM(Slide);
   RESULT:=StatusZipFileErr;
   EXIT;
  END;

  SwapLocalFileHeader(LocalFileHeader);

  IF LocalFileHeader.Signature<>BeRoZIPLocalFileHeaderSignature THEN BEGIN
   FREEMEM(Slide);
   RESULT:=StatusZipFileErr;
   EXIT;
  END;

  BitsFlagsType:=LocalFileHeader.BitFlags;

  INC(Offset,LocalFileHeader.FileNameLength+LocalFileHeader.ExtraFieldLength+SIZEOF(TBeRoZIPLocalFileHeader));
  IF (BitsFlagsType AND 8)=0 THEN BEGIN
   CompressedSize:=LocalFileHeader.CompressedSize;
   UncompressedSize:=LocalFileHeader.UncompressedSize;
   OriginalCRC:=LocalFileHeader.CRC32;
  END ELSE BEGIN
   CompressedSize:=MAXLONGINT;
   UncompressedSize:=MAXLONGINT;
   OriginalCRC:=0
  END;

  CompressMethod:=LocalFileHeader.CompressMethod;

  IF ((1 SHL CompressMethod) AND SupportedMethods)=0 THEN BEGIN
   FREEMEM(Slide);
   RESULT:=StatusNotSupported;
   EXIT;
  END;

  IF (BitsFlagsType AND 1)<>0 THEN BEGIN
   FREEMEM(Slide);
   RESULT:=StatusEncrypted;
   EXIT;
  END;

  ReachedSize:=0;
  InDataStream.Seek(Offset);

  OutDataStream:=OutStream;
  OutDataStream.Clear;
  OutDataStream.Seek(0);

  UserAbort:=FALSE;
  ItIsAtEnd:=FALSE;

  InitCRC32(CRC32Value);

  CASE CompressMethod OF
   0:RESULT:=CopyStored;
   1:RESULT:=Unshrink;
   6:RESULT:=Explode;
   8:RESULT:=Inflate;
   ELSE RESULT:=StatusNotSupported;
  END;

  IF (RESULT=Statusok) AND ((BitsFlagsType AND 8)<>0) THEN BEGIN
   DumpBits(BitsInBitBuffer AND 7);
   NeedBits(16);

   DumpBits(16);
   NeedBits(16);
   OriginalCRC:=(BitBuffer AND $FFFF) SHL 16;
   DumpBits(16);
  END;

  DoneCRC32(CRC32Value);

  IF RESULT<>0 THEN BEGIN
   OutDataStream.Clear;
  END ELSE IF OriginalCRC<>CRC32Value THEN BEGIN
   RESULT:=StatusCRCErr;
   OutDataStream.Clear;
  END;
  FREEMEM(Slide);
 END;

BEGIN
 RESULT:=DoDecompress(InStream,OutStream,HeaderOffset)=StatusOk;
END;

END.


