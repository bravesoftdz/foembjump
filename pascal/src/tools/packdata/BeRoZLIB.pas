UNIT BeRoZLIB;
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

USES BeRoStream,BeRoUtils;

FUNCTION ZCompressStream(InStream,OutStream:TBeRoStream;Level:INTEGER=6):BOOLEAN;

IMPLEMENTATION

USES BeRoZLIBCore;

FUNCTION ZCompressStream(InStream,OutStream:TBeRoStream;Level:INTEGER=6):BOOLEAN;
CONST BufferSize=32768;
TYPE PBuffer=^TBuffer;
     TBuffer=ARRAY[0..BufferSize-1] OF CHAR;
VAR zstream:z_stream;
    zresult:INTEGER;
    InBuffer:PBuffer;
    OutBuffer:PBuffer;
    InSize:INTEGER;
    OutSize:INTEGER;
BEGIN
 RESULT:=FALSE;
 TRY
  NEW(InBuffer);
  NEW(OutBuffer);
  FastFillChar(zstream,SIZEOF(z_stream),#0);
  IF DeflateInit(zstream,Level)<0 THEN EXIT;
  InStream.Seek(0);
  InSize:=InStream.Read(InBuffer^,BufferSize);
  WHILE InSize>0 DO BEGIN
   zstream.next_in:=POINTER(InBuffer);
   zstream.avail_in:=InSize;
   REPEAT
    zstream.next_out:=POINTER(OutBuffer);
    zstream.avail_out:=BufferSize;
    IF deflate(zstream,Z_NO_FLUSH)<0 THEN EXIT;
    OutSize:=BufferSize-zstream.avail_out;
    OutStream.Write(OutBuffer^,OutSize);
   UNTIL (zstream.avail_in=0) AND (zstream.avail_out>0);
   InSize:=InStream.Read(InBuffer^,BufferSize);
  END;
  REPEAT
   zstream.next_out:=POINTER(OutBuffer);
   zstream.avail_out:=BufferSize;
   zresult:=deflate(zstream,Z_FINISH);
   IF zresult<0 THEN EXIT;
   OutSize:=BufferSize-zstream.avail_out;
   OutStream.Write(OutBuffer^,OutSize);
  UNTIL (zresult=Z_STREAM_END) AND (zstream.avail_out>0);
  RESULT:=deflateEnd(zstream)>=0;
  DISPOSE(InBuffer);
  DISPOSE(OutBuffer);
 EXCEPT
 END;
END;

END.

