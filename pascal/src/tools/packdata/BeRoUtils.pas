UNIT BeRoUtils;
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
{$ELSE}
 {$DEFINE LITTLE_ENDIAN}
 {$IFNDEF CPU64}
  {$DEFINE CPU32}
 {$ENDIF}
 {$OPTIMIZATION ON}
{$ENDIF}

INTERFACE

{$IFDEF WIN32}
USES Windows;
{$ELSE}
USES SysUtils;
{$ENDIF}

TYPE TCharSet=SET OF CHAR;
     TAlphabet=ARRAY['A'..'Z'] OF CHAR;

     TFileName=STRING;

     DWORD=LONGWORD;

     Int64Rec=PACKED RECORD
      Lo,Hi:DWORD;
     END;

     LongRec=PACKED RECORD
      Lo,Hi:WORD;
     END;

{$IFDEF WIN32}
     TSearchRec=RECORD
      Time,Size,Attr:INTEGER;
      Name:TFileName;
      ExcludeAttr:INTEGER;
      FindHandle:THandle;
      FindData:TWin32FindData;
     END;
{$ELSE}
     TSearchRec=SysUtils.TSearchRec;
{$ENDIF}

CONST Alphabet:TCharSet=['A'..'Z'];
      SmallCaps:TAlphabet=('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z');
      SentenceChars:TCharSet=['.',',',';','!','?'];
      SearchSentenceChars:TCharSet=['.','!','?'];
      SpaceChars:TCharSet=[#0..#32];

      DirSplashChar={$IFDEF unixversion}'/'{$ELSE}'\'{$ENDIF};
      DirSplashNotChar={$IFNDEF unixversion}'/'{$ELSE}'\'{$ENDIF};

      fmOpenRead=$0000;
      fmOpenWrite=$0001;
      fmOpenReadWrite=$0002;
      fmShareCompat=$0000;
      fmShareExclusive=$0010;
      fmShareDenyWrite=$0020;
      fmShareDenyRead=$0030;
      fmShareDenyNone=$0040;

      faReadOnly=$00000001;
      faHidden=$00000002;
      faSysFile=$00000004;
      faVolumeID=$00000008;
      faDirectory=$00000010;
      faArchive=$00000020;
      faAnyFile=$0000003F;

FUNCTION Parse(VAR S:STRING;C:TCharSet;Continuous:BOOLEAN=FALSE):STRING; OVERLOAD;
FUNCTION Parse(VAR S:STRING;C:CHAR;Continuous:BOOLEAN=FALSE):STRING; OVERLOAD;
FUNCTION STRTOINT(S:STRING):INT64;
FUNCTION INTTOSTR(I:INT64):STRING;
FUNCTION STRTOFLOAT(S:STRING):EXTENDED;
FUNCTION FLOATTOSTR(F:EXTENDED):STRING;
FUNCTION StrLCopy(Dest:PCHAR;CONST Source:PCHAR;MaxLen:LONGWORD):PCHAR;
FUNCTION StrPCopy(Dest:PCHAR;CONST Source:STRING):PCHAR;
FUNCTION FindNext(VAR F:TSearchRec):INTEGER;
PROCEDURE FindClose(VAR F:TSearchRec);
FUNCTION FindFirst(CONST Path:STRING;Attr:INTEGER;VAR F:TSearchRec):INTEGER;
FUNCTION FILEEXISTS(S:STRING):BOOLEAN;
FUNCTION ExtractFileExt(S:STRING):STRING;
FUNCTION ExtractFileName(S:STRING):STRING;
FUNCTION ExtractFilePath(S:STRING):STRING;
FUNCTION ChangeFileExt(S,E:STRING):STRING;
FUNCTION TRIM(CONST S:STRING):STRING;
FUNCTION TRIMLEFT(CONST S:STRING):STRING;
FUNCTION TRIMRIGHT(CONST S:STRING):STRING;
FUNCTION UPPERCASE(CONST S:STRING):STRING;
FUNCTION LOWERCASE(CONST S:STRING):STRING;
FUNCTION UPCASE(CONST C:CHAR):CHAR;
FUNCTION LOCASE(CONST C:CHAR):CHAR;
FUNCTION StringReplace(VAR S:STRING;CONST FindStr,RepStr:STRING):BOOLEAN;
FUNCTION StringReplaceAll(VAR S:STRING;CONST FindStr,RepStr:STRING):BOOLEAN;
FUNCTION MatchPattern(Input,Pattern:PCHAR):BOOLEAN;
FUNCTION StrToAddr(S:STRING):INTEGER;
FUNCTION AddrToStr(Address:INTEGER):STRING;
PROCEDURE FastFillChar(VAR Dest;Count:INTEGER;Value:CHAR);
FUNCTION AreBytesEqual(CONST A,B;Count:INTEGER):BOOLEAN;
FUNCTION GetNoteDuration(NoteType,ShortestTime:INTEGER;Dots:INTEGER=0):INTEGER;
FUNCTION GCD(A,B:INTEGER):INTEGER;
FUNCTION Swap16(CONST Value:WORD):WORD;
FUNCTION Swap32(CONST Value:LONGWORD):LONGWORD; 
FUNCTION SwapWordLittleEndian(Value:WORD):WORD; {$IFDEF FPC}{INLINE;}{$ELSE}REGISTER;{$ENDIF}
FUNCTION SwapDWordLittleEndian(Value:LONGWORD):LONGWORD; {$IFDEF FPC}{INLINE;}{$ELSE}REGISTER;{$ENDIF}
FUNCTION SwapWordBigEndian(Value:WORD):WORD; {$IFDEF FPC}{INLINE;}{$ELSE}REGISTER;{$ENDIF}
FUNCTION SwapDWordBigEndian(Value:LONGWORD):LONGWORD; {$IFDEF FPC}{INLINE;}{$ELSE}REGISTER;{$ENDIF}
PROCEDURE SwapLittleEndianData16(VAR Data); {$IFDEF FPC}{INLINE;}{$ELSE}REGISTER;{$ENDIF}
PROCEDURE SwapLittleEndianData32(VAR Data); {$IFDEF FPC}{INLINE;}{$ELSE}REGISTER;{$ENDIF}
PROCEDURE SwapBigEndianData16(VAR Data); {$IFDEF FPC}{INLINE;}{$ELSE}REGISTER;{$ENDIF}
PROCEDURE SwapBigEndianData32(VAR Data); {$IFDEF FPC}{INLINE;}{$ELSE}REGISTER;{$ENDIF}
FUNCTION DecToHex(Value:INT64):STRING;
FUNCTION HexToDec(Value:STRING):INT64;
FUNCTION ByteToHex(Value:BYTE):STRING;
FUNCTION HexToByte(Value:STRING):BYTE;
FUNCTION WordToHex(Value:WORD):STRING;
FUNCTION HexToWord(Value:STRING):WORD;
FUNCTION LongWordToHex(Value:LONGWORD):STRING;
FUNCTION HexToLongWord(Value:STRING):LONGWORD;
FUNCTION FABS(Value:SINGLE):SINGLE;

IMPLEMENTATION

CONST HexNumbers:ARRAY[0..$F] OF CHAR='0123456789ABCDEF';

FUNCTION Parse(VAR S:STRING;C:TCharSet;Continuous:BOOLEAN=FALSE):STRING; OVERLOAD;
VAR Counter,Index:INTEGER;
BEGIN
 Index:=0;
 FOR Counter:=1 TO LENGTH(S) DO BEGIN
  IF S[Counter] IN C THEN BEGIN
   Index:=Counter;
   BREAK;
  END;
 END;
 IF Index<>0 THEN BEGIN
  RESULT:=COPY(S,1,Index-1);
  DELETE(S,1,Index);
 END ELSE BEGIN
  RESULT:=S;
  S:='';
 END;
 IF Continuous THEN WHILE (LENGTH(S)>0) AND (S[1] IN C) DO DELETE(S,1,1);
END;

FUNCTION Parse(VAR S:STRING;C:CHAR;Continuous:BOOLEAN=FALSE):STRING; OVERLOAD;
VAR Index:INTEGER;
BEGIN
 Index:=POS(C,S);;
 IF Index<>0 THEN BEGIN
  RESULT:=COPY(S,1,Index-1);
  DELETE(S,1,Index);
 END ELSE BEGIN
  RESULT:=S;
  S:='';
 END;
 IF Continuous THEN WHILE (LENGTH(S)>0) AND (S[1]=C) DO DELETE(S,1,1);
END;

FUNCTION STRTOINT(S:STRING):INT64;
VAR Code:INTEGER;
BEGIN
 VAL(S,RESULT,Code);
END;

FUNCTION INTTOSTR(I:INT64):STRING;
BEGIN
 STR(I,RESULT);
END;

FUNCTION STRTOFLOAT(S:STRING):EXTENDED;
VAR Code:INTEGER;
BEGIN
 VAL(S,RESULT,Code);
END;

FUNCTION FLOATTOSTR(F:EXTENDED):STRING;
BEGIN
 IF F=TRUNC(F) THEN BEGIN
  RESULT:=INTTOSTR(TRUNC(F));
 END ELSE BEGIN
  STR(F,RESULT);
  RESULT:=TRIM(RESULT);
 END;
END;

FUNCTION StrLCopy(Dest:PCHAR;CONST Source:PCHAR;MaxLen:LONGWORD):PCHAR;{$IFDEF CPU386} ASSEMBLER;
ASM
 PUSH EDI
 PUSH ESI
 PUSH EBX
 MOV ESI,EAX

 MOV EDI,EDX
 MOV EBX,ECX
 XOR AL,AL
 TEST ECX,ECX
 JZ @@1
 REPNE SCASB
 JNE @@1
 INC ECX
@@1:
 SUB EBX,ECX
 MOV EDI,ESI
 MOV ESI,EDX
 MOV EDX,EDI
 MOV ECX,EBX
 SHR ECX,2
 REP MOVSD
 MOV ECX,EBX
 AND ECX,3
 REP MOVSB
 STOSB
 MOV EAX,EDX
 POP EBX
 POP ESI
 POP EDI
END;
{$ELSE}
BEGIN
 IF MaxLen=0 THEN BEGIN
  MOVE(Source^,Dest^,LENGTH(Source));
 END ELSE BEGIN
  MOVE(Source^,Dest^,MaxLen);
  Dest[MaxLen]:=#0;
 END;
END;
{$ENDIF}

FUNCTION StrPCopy(Dest:PCHAR;CONST Source:STRING):PCHAR;
BEGIN
 RESULT:=StrLCopy(Dest,PCHAR(Source),LENGTH(Source));
ENd;

{$IFDEF WIN32}
FUNCTION FindMatchingFile(VAR F:TSearchRec):INTEGER;
VAR LocalFileTime:TFileTime;
BEGIN
 WITH F DO BEGIN
  WHILE (FindData.dwFileAttributes AND ExcludeAttr)<>0 DO BEGIN
   IF NOT Windows.FindNextFile(FindHandle,FindData) THEN BEGIN
    RESULT:=GetLastError;
    EXIT;
   END;
  END;
  FileTimeToLocalFileTime(FindData.ftLastWriteTime,LocalFileTime);
  FileTimeToDosDateTime(LocalFileTime,LongRec(Time).Hi,LongRec(Time).Lo);
  Size:=FindData.nFileSizeLow;
  Attr:=FindData.dwFileAttributes;
  Name:=FindData.cFileName;
 END;
 RESULT:=0;
END;
{$ENDIF}

FUNCTION FindNext(VAR F:TSearchRec):INTEGER;
BEGIN
{$IFDEF WIN32}
 IF Windows.FindNextFile(F.FindHandle,F.FindData) THEN BEGIN
  RESULT:=FindMatchingFile(F);
 END ELSE BEGIN
  RESULT:=GetLastError;
 END;
{$ELSE}
 RESULT:=SysUtils.FindNext(F);
{$ENDIF}
END;

PROCEDURE FindClose(VAR F:TSearchRec);
BEGIN
{$IFDEF WIN32}
 IF F.FindHandle<>INVALID_HANDLE_VALUE THEN BEGIN
  Windows.FindClose(F.FindHandle);
  F.FindHandle:=INVALID_HANDLE_VALUE;
 END;
{$ELSE}
 SysUtils.FindClose(F);
{$ENDIF}
END;

FUNCTION FindFirst(CONST Path:STRING;Attr:INTEGER;VAR F:TSearchRec):INTEGER;
{$IFDEF WIN32}
CONST faSpecial=faHidden OR faSysFile OR faVolumeID OR faDirectory;
{$ENDIF}
BEGIN
{$IFDEF WIN32}
 F.ExcludeAttr:=NOT Attr AND faSpecial;
 F.FindHandle:=Windows.FindFirstFile(PCHAR(Path),F.FindData);
 IF F.FindHandle<>INVALID_HANDLE_VALUE THEN BEGIN
  RESULT:=FindMatchingFile(F);
  IF RESULT<>0 THEN FindClose(F);
 END ELSE BEGIN
  RESULT:=GetLastError;
 END;
{$ELSE}
 RESULT:=SysUtils.FindFirst(Path,Attr,F);
{$ENDIF}
END;

FUNCTION FILEEXISTS(S:STRING):BOOLEAN;
VAR F:FILE;
BEGIN
 RESULT:=FALSE;
 ASSIGNFILE(F,S);
 {$I-}RESET(F,1);{$I+}
 IF IOResult=0 THEN BEGIN
  CLOSEFILE(F);
  RESULT:=TRUE;
 END;
END;

FUNCTION ExtractFileExt(S:STRING):STRING;
VAR I,J,K:INTEGER;
BEGIN
 RESULT:='';
 K:=0;
 J:=LENGTH(S);
 FOR I:=J DOWNTO 1 DO IF (S[I]='.') OR (S[I]='\') OR (S[I]='/') OR (S[I]=':') THEN BEGIN
  K:=I;
  BREAK;
 END;
 IF (K>0) AND (S[K]='.') THEN RESULT:=COPY(S,K,J-K+1);
END;

FUNCTION ExtractFileName(S:STRING):STRING;
VAR I,J,K:INTEGER;
BEGIN
 RESULT:=S;
 K:=0;
 J:=LENGTH(S);
 FOR I:=J DOWNTO 1 DO IF (S[I]='\') OR (S[I]='/') OR (S[I]=':') THEN BEGIN
  K:=I;
  BREAK;
 END;
 IF K>0 THEN RESULT:=COPY(S,K+1,J-K+1);
END;

FUNCTION ExtractFilePath(S:STRING):STRING;
VAR I,J,K:INTEGER;
BEGIN
 RESULT:=S;
 K:=0;
 J:=LENGTH(S);
 FOR I:=J DOWNTO 1 DO IF (S[I]='\') OR (S[I]='/') OR (S[I]=':') THEN BEGIN
  K:=I;
  BREAK;
 END;
 IF K>0 THEN RESULT:=COPY(S,1,K);
END;

FUNCTION ChangeFileExt(S,E:STRING):STRING;
VAR I,J,K:INTEGER;
BEGIN
 K:=0;
 J:=LENGTH(S);
 FOR I:=J DOWNTO 1 DO IF (S[I]='.') OR (S[I]='\') OR (S[I]='/') OR (S[I]=':') THEN BEGIN
  K:=I;
  BREAK;
 END;
 IF (K>0) AND (S[K]='.') THEN BEGIN
  RESULT:=COPY(S,1,K-1)+E;
 END ELSE BEGIN
  RESULT:=S+E;
 END;
END;

FUNCTION TRIM(CONST S:STRING):STRING;
VAR StartPosition,LengthCount:INTEGER;
BEGIN
 LengthCount:=LENGTH(S);
 IF LengthCount>0 THEN BEGIN
  WHILE (LengthCount>0) AND (S[LengthCount] IN [#0..#32]) DO DEC(LengthCount);
  StartPosition:=1;
  WHILE (StartPosition<=LengthCount) AND (S[StartPosition] IN [#0..#32]) DO INC(StartPosition);
  RESULT:=COPY(S,StartPosition,LengthCount-StartPosition+1);
 END ELSE BEGIN
  RESULT:='';
 END;
END;

FUNCTION TRIMLEFT(CONST S:STRING):STRING;
VAR StartPosition,LengthCount:INTEGER;
BEGIN
 LengthCount:=LENGTH(S);
 IF LengthCount>0 THEN BEGIN
  StartPosition:=1;
  WHILE (StartPosition<=LengthCount) AND (S[StartPosition] IN [#0..#32]) DO INC(StartPosition);
  RESULT:=COPY(S,StartPosition,LengthCount-StartPosition+1);
 END ELSE BEGIN
  RESULT:='';
 END;
END;

FUNCTION TRIMRIGHT(CONST S:STRING):STRING;
VAR StartPosition,LengthCount:INTEGER;
BEGIN
 LengthCount:=LENGTH(S);
 IF LengthCount>0 THEN BEGIN
  WHILE (LengthCount>0) AND (S[LengthCount] IN [#0..#32]) DO DEC(LengthCount);
  StartPosition:=1;
  RESULT:=COPY(S,StartPosition,LengthCount-StartPosition+1);
 END ELSE BEGIN
  RESULT:='';
 END;
END;

FUNCTION UPPERCASE(CONST S:STRING):STRING;
VAR I,L:INTEGER;
BEGIN
 RESULT:='';
 L:=LENGTH(S);
 I:=1;
 WHILE I<=L DO BEGIN
  IF S[I] IN ['a'..'z'] THEN BEGIN
   RESULT:=RESULT+CHAR(BYTE(S[I])-32);
  END ELSE BEGIN
   RESULT:=RESULT+S[I];
  END;
  INC(I);
 END;
END;

FUNCTION LOWERCASE(CONST S:STRING):STRING;
VAR I,L:INTEGER;
BEGIN
 RESULT:='';
 L:=LENGTH(S);
 I:=1;
 WHILE I<=L DO BEGIN
  IF S[I] IN ['A'..'Z'] THEN BEGIN
   RESULT:=RESULT+CHAR(BYTE(S[I])+32);
  END ELSE BEGIN
   RESULT:=RESULT+S[I];
  END;
  INC(I);
 END;
END;

FUNCTION UPCASE(CONST C:CHAR):CHAR;
BEGIN
 IF C IN ['a'..'z'] THEN BEGIN
  RESULT:=CHAR(BYTE(C)-32);
 END ELSE BEGIN
  RESULT:=C;
 END;
END;

FUNCTION LOCASE(CONST C:CHAR):CHAR;
BEGIN
 IF C IN ['A'..'Z'] THEN BEGIN
  RESULT:=CHAR(BYTE(C)+32);
 END ELSE BEGIN
  RESULT:=C;
 END;
END;

FUNCTION StringReplace(VAR S:STRING;CONST FindStr,RepStr:STRING):BOOLEAN;
VAR Index:INTEGER;
BEGIN
 RESULT:=FALSE;
 IF POS(FindStr,RepStr)=0 THEN BEGIN
  Index:=POS(FindStr,S);
  IF Index<>0 THEN BEGIN
   S:=COPY(S,1,Index-1)+RepStr+COPY(S,Index+LENGTH(FindStr),LENGTH(S));
   RESULT:=TRUE;
  END;
 END;
END;

FUNCTION StringReplaceAll(VAR S:STRING;CONST FindStr,RepStr:STRING):BOOLEAN;
BEGIN
 RESULT:=FALSE;
 WHILE StringReplace(S,FindStr,RepStr) DO RESULT:=TRUE;
END;

FUNCTION MatchPattern(Input,Pattern:PCHAR):BOOLEAN;
BEGIN
 RESULT:=TRUE;
 WHILE TRUE DO BEGIN
  CASE Pattern[0] OF
   #0:BEGIN
    RESULT:=Input[0]=#0;
    EXIT;
   END;
   '*':BEGIN
    INC(Pattern);
    IF Pattern[0]=#0 THEN BEGIN
     RESULT:=TRUE;
     EXIT;
    END;
    WHILE Input[0]<>#0 DO BEGIN
     IF MatchPattern(Input,Pattern) THEN BEGIN
      RESULT:=TRUE;
      EXIT;
     END;
     INC(Input);
    END;
   END;
   '?':BEGIN
    IF Input[0]=#0 THEN BEGIN
     RESULT:=FALSE;
     EXIT;
    END;
    INC(Input);
    INC(Pattern);
   END;
   '[':BEGIN
    IF Pattern[1] IN [#0,'[',']'] THEN BEGIN
     RESULT:=FALSE;
     EXIT;
    END;
    IF Pattern[1]='^' THEN BEGIN
     INC(Pattern,2);
     RESULT:=TRUE;
     WHILE Pattern[0]<>']' DO BEGIN
      IF Pattern[1]='-' THEN BEGIN
       IF (Input[0]>=Pattern[0]) AND (Input[0]<=Pattern[2]) THEN BEGIN
        RESULT:=FALSE;
        BREAK;
       END ELSE BEGIN
        INC(Pattern,3);
       END;
      END ELSE BEGIN
       IF Input[0]=Pattern[0] THEN BEGIN
        RESULT:=FALSE;
        BREAK;
       END ELSE BEGIN
        INC(Pattern);
       END;
      END;
     END;
    END ELSE BEGIN
     INC(Pattern);
     RESULT:=FALSE;
     WHILE Pattern[0]<>']' DO BEGIN
      IF Pattern[1]='-' THEN BEGIN
       IF (Input[0]>=Pattern[0]) AND (Input[0]<=Pattern[2]) THEN BEGIN
        RESULT:=TRUE;
        BREAK;
       END ELSE BEGIN
        Inc(Pattern,3);
       END;
      END ELSE BEGIN
       IF Input[0]=Pattern[0] THEN BEGIN
        RESULT:=TRUE;
        BREAK;
       END ELSE BEGIN
        INC(Pattern);
       END;
      END;
     END;
    END;
    IF RESULT THEN BEGIN
     INC(Input);
     WHILE NOT (Pattern[0] IN [']',#0]) DO INC(Pattern);
     IF Pattern[0]=#0 THEN BEGIN
      RESULT:=FALSE;
      EXIT;
     END ELSE BEGIN
      INC(Pattern);
     END;
    END ELSE BEGIN
     EXIT;
    END;
   END;
   ELSE BEGIN
    IF Input[0]<>Pattern[0] THEN BEGIN
     RESULT:=FALSE;
     BREAK;
    END;
    INC(Input);
    INC(Pattern);
   END;
  END;
 END;
END;

FUNCTION StrToAddr(S:STRING):INTEGER;
VAR R,I,P,C:INTEGER;
    T:STRING;
BEGIN
 RESULT:=0;
 R:=0;
 FOR I:=0 TO 3 DO BEGIN
  P:=POS('.',S);
  IF P=0 THEN P:=LENGTH(S)+1;
  IF P<=1 THEN EXIT;
  T:=COPY(S,1,P-1);
  DELETE(S,1,P);
  VAL(T,P,C);
  IF (C<>0) OR (P<0) OR (P>255) THEN EXIT;
  R:=R OR P SHL (I*8);
 END;
 RESULT:=R;
END;

FUNCTION AddrToStr(Address:INTEGER):STRING;
VAR R,S:STRING;
    I:INTEGER;
BEGIN
 R:='';
 FOR I:=0 TO 3 DO BEGIN
  STR(Address SHR (I*8) AND $FF,S);
  R:=R+S;
  IF I<3 THEN R:=R+'.';
 END;
 RESULT:=R;
END;

TYPE PInteger=^INTEGER;
     PIntegerArray=^TIntegerArray;
     TIntegerArray=ARRAY[0..($7FFFFFFF DIV SIZEOF(INTEGER))-1] OF INTEGER;

     PByte=^BYTE;
     PByteArray=^TByteArray;
     TByteArray=ARRAY[0..($7FFFFFFF DIV SIZEOF(BYTE))-1] OF BYTE;

PROCEDURE FastFillChar(VAR Dest;Count:INTEGER;Value:CHAR);{$IFDEF CPU64}
BEGIN
 FILLCHAR(Dest,Count,Value);
END;
{$ELSE}
LABEL P01,P02,P03,P04,P05,P06,P07,P08,P09,P10,P11,P12;
VAR I,J,K:INTEGER;
    P:POINTER;
BEGIN
 IF Count>0 THEN BEGIN
  P:=@Dest;
  IF Count>=12 THEN BEGIN
   J:=BYTE(Value);
   J:=J OR (J SHL 8);
   J:=J OR (J SHL 16);
   PInteger(P)^:=J;
   PInteger(INTEGER(P)+Count-4)^:=J;
   I:=Count SHR 2;
   IF Count>=256 THEN BEGIN
    IF Count<448 THEN BEGIN
     PIntegerArray(P)[1]:=J;
     PIntegerArray(P)[2]:=J;
     PIntegerArray(P)[3]:=J;
     REPEAT
      DEC(I,4);
      PIntegerArray(P)[I]:=J;
      PIntegerArray(P)[I+1]:=J;
      PIntegerArray(P)[I+2]:=J;
      PIntegerArray(P)[I+3]:=J;
     UNTIL I<4;
    END ELSE BEGIN
     I:=Count;
     K:=(INTEGER(P) AND 3)-4;
     DEC(I,16);
     DEC(PByte(P),K);
     INC(I,K);
     INC(PByte(P),I);
     PIntegerArray(P)[0] := J;
     PIntegerArray(P)[1] := J;
     PIntegerArray(P)[2] := J;
     PIntegerArray(P)[3] := J;
     REPEAT
      PIntegerArray(INTEGER(P)-I)[0]:=J;
      PIntegerArray(INTEGER(P)-I)[1]:=J;
      PIntegerArray(INTEGER(P)-I)[2]:=J;
      PIntegerArray(INTEGER(P)-I)[3]:=J;
      DEC(I,16);
     UNTIL I<=0;
    END;
   END ELSE BEGIN
    REPEAT
     DEC(I,2);
     PIntegerArray(P)[I]:=J;
     PIntegerArray(P)[I+1]:=J;
    UNTIL I<2;
   END;
  END ELSE BEGIN
   CASE Count OF
    1:GOTO P01;
    2:GOTO P02;
    3:GOTO P03;
    4:GOTO P04;
    5:GOTO P05;
    6:GOTO P06;
    7:GOTO P07;
    8:GOTO P08;
    9:GOTO P09;
    10:GOTO P10;
    11:GOTO P11;
    12:GOTO P12;
   END;
   P12:PByteArray(P)[11]:=BYTE(Value);
   P11:PByteArray(P)[10]:=BYTE(Value);
   P10:PByteArray(P)[09]:=BYTE(Value);
   P09:PByteArray(P)[08]:=BYTE(Value);
   P08:PByteArray(P)[07]:=BYTE(Value);
   P07:PByteArray(P)[06]:=BYTE(Value);
   P06:PByteArray(P)[05]:=BYTE(Value);
   P05:PByteArray(P)[04]:=BYTE(Value);
   P04:PByteArray(P)[03]:=BYTE(Value);
   P03:PByteArray(P)[02]:=BYTE(Value);
   P02:PByteArray(P)[01]:=BYTE(Value);
   P01:PByteArray(P)[00]:=BYTE(Value);
  END;
 END;
END;
{$ENDIF}

FUNCTION AreBytesEqual(CONST A,B;Count:INTEGER):BOOLEAN;
VAR FirstComparePointer,SecondComparePointer:PBYTE;
    Counter:INTEGER;
BEGIN
 TRY
  RESULT:=TRUE;
  FirstComparePointer:=@A;
  SecondComparePointer:=@B;
  FOR Counter:=1 TO Count DO BEGIN
   IF FirstComparePointer^<>SecondComparePointer^ THEN BEGIN
    RESULT:=FALSE;
    EXIT;
   END;
   INC(FirstComparePointer);
   INC(SecondComparePointer);
  END;
 EXCEPT
  RESULT:=FALSE;
 END;
END;

FUNCTION GetNoteDuration(NoteType,ShortestTime:INTEGER;Dots:INTEGER=0):INTEGER;
VAR Duration,Extra,DotCouunter:INTEGER;
BEGIN
 Duration:=ShortestTime*(1 SHL NoteType);
 IF Dots<>0 THEN BEGIN
  Extra:=Duration DIV 2;
  FOR DotCouunter:=1 TO Dots DO BEGIN
   INC(Duration,Extra);
   Extra:=Extra DIV 2;
  END;
 END;
 RESULT:=Duration;
END;

FUNCTION GCD(A,B:INTEGER):INTEGER;
BEGIN
 IF A=0 THEN B:=A;
 IF B=0 THEN A:=B;
 WHILE A<>B DO BEGIN
  IF A>B THEN DEC(A,B);
  IF B>A THEN DEC(B,A);
 END;
 IF A=0 THEN A:=1;
 RESULT:=A;
END;

FUNCTION Swap16(CONST Value:WORD):WORD;
BEGIN
{$IFDEF CPU386}
 RESULT:=((Value AND $FF) SHL 8) OR ((Value AND $FF00) SHR 8);
{$ELSE}
 RESULT:=Value;
{$ENDIF}
END;

FUNCTION Swap32(CONST Value:LONGWORD):LONGWORD;
BEGIN
{$IFDEF CPU386}
 RESULT:=((Value AND $FF) SHL 24) OR (((Value AND $FF00) SHR 8) SHL 16) OR (((Value AND $FF0000) SHR 16) SHL 8) OR ((Value AND $FF000000) SHR 24);
{$ELSE}
 RESULT:=Value;
{$ENDIF}
END;

FUNCTION SwapWordLittleEndian(Value:WORD):WORD; {$IFDEF FPC}{INLINE;}{$ELSE}REGISTER;{$ENDIF}
BEGIN
{$IFDEF BIG_ENDIAN}
 RESULT:=((Value AND $FF00) SHR 8) OR ((Value AND $FF) SHL 8);
{$ELSE}
 RESULT:=Value;
{$ENDIF}
END;

FUNCTION SwapDWordLittleEndian(Value:LONGWORD):LONGWORD; {$IFDEF FPC}{INLINE;}{$ELSE}REGISTER;{$ENDIF}
BEGIN
{$IFDEF BIG_ENDIAN}
 RESULT:=((Value AND $FF000000) SHR 24) OR ((Value AND $00FF0000) SHR 8) OR
         ((Value AND $0000FF00) SHL 8) OR ((Value AND $000000FF) SHL 24);
{$ELSE}
 RESULT:=Value;
{$ENDIF}
END;

FUNCTION SwapWordBigEndian(Value:WORD):WORD; {$IFDEF FPC}{INLINE;}{$ELSE}REGISTER;{$ENDIF}
BEGIN
{$IFDEF LITTLE_ENDIAN}
 RESULT:=((Value AND $FF00) SHR 8) OR ((Value AND $FF) SHL 8);
{$ELSE}
 RESULT:=Value;
{$ENDIF}
END;

FUNCTION SwapDWordBigEndian(Value:LONGWORD):LONGWORD; {$IFDEF FPC}{INLINE;}{$ELSE}REGISTER;{$ENDIF}
BEGIN
{$IFDEF LITTLE_ENDIAN}
 RESULT:=((Value AND $FF000000) SHR 24) OR ((Value AND $00FF0000) SHR 8) OR
         ((Value AND $0000FF00) SHL 8) OR ((Value AND $000000FF) SHL 24);
{$ELSE}
 RESULT:=Value;
{$ENDIF}
END;

PROCEDURE SwapLittleEndianData16(VAR Data); {$IFDEF FPC}{INLINE;}{$ELSE}REGISTER;{$ENDIF}
{$IFDEF BIG_ENDIAN}
VAR Value:WORD ABSOLUTE Data;
BEGIN
 Value:=((Value AND $FF00) SHR 8) OR ((Value AND $FF) SHL 8);
{$ELSE}
BEGIN
{$ENDIF}
END;

PROCEDURE SwapLittleEndianData32(VAR Data); {$IFDEF FPC}{INLINE;}{$ELSE}REGISTER;{$ENDIF}
{$IFDEF BIG_ENDIAN}
VAR Value:LONGWORD ABSOLUTE Data;
BEGIN
 Value:=((Value AND $FF000000) SHR 24) OR ((Value AND $00FF0000) SHR 8) OR
        ((Value AND $0000FF00) SHL 8) OR ((Value AND $000000FF) SHL 24);
{$ELSE}
BEGIN
{$ENDIF}
END;

PROCEDURE SwapBigEndianData16(VAR Data); {$IFDEF FPC}{INLINE;}{$ELSE}REGISTER;{$ENDIF}
{$IFDEF LITTLE_ENDIAN}
VAR Value:WORD ABSOLUTE Data;
BEGIN
 Value:=((Value AND $FF00) SHR 8) OR ((Value AND $FF) SHL 8);
{$ELSE}
BEGIN
 RESULT:=Value;
{$ENDIF}
END;

PROCEDURE SwapBigEndianData32(VAR Data); {$IFDEF FPC}{INLINE;}{$ELSE}REGISTER;{$ENDIF}
{$IFDEF LITTLE_ENDIAN}
VAR Value:LONGWORD ABSOLUTE Data;
BEGIN
 Value:=((Value AND $FF000000) SHR 24) OR ((Value AND $00FF0000) SHR 8) OR
        ((Value AND $0000FF00) SHL 8) OR ((Value AND $000000FF) SHL 24);
{$ELSE}
BEGIN
{$ENDIF}
END;

FUNCTION DecToHex(Value:INT64):STRING;
BEGIN
 RESULT:='';
 WHILE Value<>0 DO BEGIN
  RESULT:=HexNumbers[Value AND $F]+RESULT;
  Value:=Value SHR 4;
 END;
END;

FUNCTION HexToDec(Value:STRING):INT64;
VAR Counter:INTEGER;
    Nibble:BYTE;
BEGIN
 RESULT:=0;
 FOR Counter:=LENGTH(Value) DOWNTO 1 DO BEGIN
  Nibble:=BYTE(Value[Counter]);
  IF (Nibble>=BYTE('0')) AND (Nibble<=BYTE('9')) THEN BEGIN
   Nibble:=Nibble-BYTE('0');
  END ELSE IF (Nibble>=BYTE('A')) AND (Nibble<=BYTE('F')) THEN BEGIN
   Nibble:=Nibble-BYTE('A')+$A;
  END ELSE IF (Nibble>=BYTE('a')) AND (Nibble<=BYTE('f')) THEN BEGIN
   Nibble:=Nibble-BYTE('a')+$A;
  END ELSE BEGIN
   Nibble:=0;
  END;
  RESULT:=(RESULT SHL 4) OR Nibble;
 END;
END;

FUNCTION ByteToHex(Value:BYTE):STRING;
BEGIN
 RESULT:='';
 WHILE Value<>0 DO BEGIN
  RESULT:=HexNumbers[Value AND $F]+RESULT;
  Value:=Value SHR 4;
 END;
 WHILE LENGTH(RESULT)<2 DO RESULT:='0'+RESULT;
END;

FUNCTION HexToByte(Value:STRING):BYTE;
VAR Counter:INTEGER;
    Nibble:BYTE;
BEGIN
 RESULT:=0;
 FOR Counter:=LENGTH(Value) DOWNTO 1 DO BEGIN
  Nibble:=BYTE(Value[Counter]);
  IF (Nibble>=BYTE('0')) AND (Nibble<=BYTE('9')) THEN BEGIN
   Nibble:=Nibble-BYTE('0');
  END ELSE IF (Nibble>=BYTE('A')) AND (Nibble<=BYTE('F')) THEN BEGIN
   Nibble:=Nibble-BYTE('A')+$A;
  END ELSE IF (Nibble>=BYTE('a')) AND (Nibble<=BYTE('f')) THEN BEGIN
   Nibble:=Nibble-BYTE('a')+$A;
  END ELSE BEGIN
   Nibble:=0;
  END;
  RESULT:=(RESULT SHL 4) OR Nibble;
 END;
END;

FUNCTION WordToHex(Value:WORD):STRING;
BEGIN
 RESULT:='';
 WHILE Value<>0 DO BEGIN
  RESULT:=HexNumbers[Value AND $F]+RESULT;
  Value:=Value SHR 4;
 END;
 WHILE LENGTH(RESULT)<4 DO RESULT:='0'+RESULT;
END;

FUNCTION HexToWord(Value:STRING):WORD;
VAR Counter:INTEGER;
    Nibble:BYTE;
BEGIN
 RESULT:=0;
 FOR Counter:=LENGTH(Value) DOWNTO 1 DO BEGIN
  Nibble:=BYTE(Value[Counter]);
  IF (Nibble>=BYTE('0')) AND (Nibble<=BYTE('9')) THEN BEGIN
   Nibble:=Nibble-BYTE('0');
  END ELSE IF (Nibble>=BYTE('A')) AND (Nibble<=BYTE('F')) THEN BEGIN
   Nibble:=Nibble-BYTE('A')+$A;
  END ELSE IF (Nibble>=BYTE('a')) AND (Nibble<=BYTE('f')) THEN BEGIN
   Nibble:=Nibble-BYTE('a')+$A;
  END ELSE BEGIN
   Nibble:=0;
  END;
  RESULT:=(RESULT SHL 4) OR Nibble;
 END;
END;

FUNCTION LongWordToHex(Value:LONGWORD):STRING; REGISTER;
BEGIN
 RESULT:='';
 WHILE Value<>0 DO BEGIN
  RESULT:=HexNumbers[Value AND $F]+RESULT;
  Value:=Value SHR 4;
 END;
 WHILE LENGTH(RESULT)<8 DO RESULT:='0'+RESULT;
END;

FUNCTION HexToLongWord(Value:STRING):LONGWORD; REGISTER;
VAR Counter:INTEGER;
    Nibble:BYTE;
BEGIN
 RESULT:=0;
 FOR Counter:=LENGTH(Value) DOWNTO 1 DO BEGIN
  Nibble:=BYTE(Value[Counter]);
  IF (Nibble>=BYTE('0')) AND (Nibble<=BYTE('9')) THEN BEGIN
   Nibble:=Nibble-BYTE('0');
  END ELSE IF (Nibble>=BYTE('A')) AND (Nibble<=BYTE('F')) THEN BEGIN
   Nibble:=Nibble-BYTE('A')+$A;
  END ELSE IF (Nibble>=BYTE('a')) AND (Nibble<=BYTE('f')) THEN BEGIN
   Nibble:=Nibble-BYTE('a')+$A;
  END ELSE BEGIN
   Nibble:=0;
  END;
  RESULT:=(RESULT SHL 4) OR Nibble;
 END;
END;

FUNCTION FABS(Value:SINGLE):SINGLE;
VAR L:LONGWORD ABSOLUTE Value;
BEGIN
 L:=L AND $7FFFFFFF;
 RESULT:=Value;
END;

END.

