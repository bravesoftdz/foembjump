UNIT Textures;
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
INTERFACE

USES GL,glext,Texture,BeRoStream,DataManager,BeRoUtils;

PROCEDURE SwapRGB(Daten:POINTER;Groesse:LONGWORD);
PROCEDURE ChangeGamma(VAR color:ARRAY OF BYTE;Faktor:SINGLE);
PROCEDURE SetTexture(Textur:glUInt;Width,Height,Format:WORD;pDaten:POINTER);
FUNCTION CreateTexture(Width,Height,Format:WORD;pDaten:POINTER):INTEGER;
FUNCTION CreateNoTexture:INTEGER;
FUNCTION LoadBMPTexture(Dateiname:STRING;VAR Textur:GLuint):BOOLEAN;
FUNCTION LoadTGATexture(Dateiname:STRING;VAR Texture:GLuint):BOOLEAN;
FUNCTION LoadTexture(Dateiname:STRING;VAR Textur:GLuint):BOOLEAN;

VAR TransparentTexture:BOOLEAN=FALSE;
    NoShaderTextur:glUInt;

IMPLEMENTATION

PROCEDURE SwapRGB(Daten:POINTER;Groesse:LONGWORD);
{$ifdef cpu386}
ASM
 MOV EBX,DWORD PTR Daten
 MOV ECX,DWORD PTR Groesse
 @Schleife:
  MOV AL,[EBX+0]
  MOV AH,[EBX+2]
  MOV [EBX+2],AL
  MOV [EBX+0],AH
  ADD EBX,3
  DEC ECX
 JNZ @Schleife
END;
{$else}
var p:pchar;
    b:char;
begin
 p:=Daten;
 while Groesse>0 do begin
  b:=p[0];
  p[0]:=p[2];
  p[2]:=b;
  inc(p,3);
  dec(Groesse);
 end;
END;
{$endif}

PROCEDURE SetTexture(Textur:glUInt;Width,Height,Format:WORD;pDaten:POINTER);
BEGIN
 glBindTexture(GL_TEXTURE_2D,Textur);
 glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
 glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
 glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
 glTexImage2D(GL_TEXTURE_2D,0,3,Width,Height,0,GL_RGB,GL_UNSIGNED_BYTE,pDaten);
END;

FUNCTION CreateTexture(Width,Height,Format:WORD;pDaten:POINTER):INTEGER;
VAR Texture:GLuint;
BEGIN
 glGenTextures(1,@Texture);
 glBindTexture(GL_TEXTURE_2D,Texture);
 glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
 glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
 glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
 IF Format=GL_RGBA THEN BEGIN
  glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA,Width,Height,0,GL_RGBA,GL_UNSIGNED_BYTE,pDaten);
//gluBuild2DMipmaps(GL_TEXTURE_2D,GL_RGBA,Width,Height,GL_RGBA,GL_UNSIGNED_BYTE,pDaten);
 END ELSE BEGIN
  TransparentTexture:=FALSE;
  glTexImage2D(GL_TEXTURE_2D,0,3,Width,Height,0,GL_RGB,GL_UNSIGNED_BYTE,pDaten);
//gluBuild2DMipmaps(GL_TEXTURE_2D,3,Width,Height,GL_RGB,GL_UNSIGNED_BYTE,pDaten);
 END;
 RESULT:=Texture;
END;

PROCEDURE ChangeGamma(VAR color:ARRAY OF BYTE;Faktor:SINGLE);
VAR scale,r,g,b:SINGLE;
BEGIN
 scale:=1;
 r:=color[0];
 g:=color[1];
 b:=color[2];

 r:=r*Faktor/255;
 g:=g*Faktor/255;
 b:=b*Faktor/255;

 IF (r>1) AND ((1/r)<scale) THEN scale:=1/r;
 IF (g>1) AND ((1/g)<scale) THEN scale:=1/g;
 IF (b>1) AND ((1/b)<scale) THEN scale:=1/b;

 scale:=scale*255.0;
 r:=r*scale;
 g:=g*scale;
 b:=b*scale;

 color[0]:=TRUNC(r);
 color[1]:=TRUNC(g);
 color[2]:=TRUNC(b);
END;

FUNCTION CreateNoTexture:INTEGER;
VAR pDaten:POINTER;
    BitmapLaenge:LONGWORD;
BEGIN
 BitmapLaenge:=128*128*3;
 GETMEM(pDaten,BitmapLaenge);
 FILLCHAR(pDaten^,BitmapLaenge,#0);
 RESULT:=CreateTexture(128,128,GL_RGB,pDaten);
END;

FUNCTION LoadBMPTexture(Dateiname:STRING;VAR Textur:GLuint):BOOLEAN;
type dword=longword;
     TBitmapFileHeader=packed record
      bfType:Word;
      bfSize:DWORD;
      bfReserved1:Word;
      bfReserved2:Word;
      bfOffBits:DWORD;
     end;
     TBitmapInfoHeader=packed record
      biSize:DWORD;
      biWidth:Longint;
      biHeight:Longint;
      biPlanes:Word;
      biBitCount:Word;
      biCompression:DWORD;
      biSizeImage:DWORD;
      biXPelsPerMeter:Longint;
      biYPelsPerMeter:Longint;
      biClrUsed:DWORD;
      biClrImportant:DWORD;
     end;
     TRGBQUAD=packed record
      rgbBlue:Byte;
      rgbGreen:Byte;
      rgbRed:Byte;
      rgbReserved:Byte;
     end;
VAR DateiHeader:TBitmapFileHeader;
    InfoHeader:TBitmapInfoHeader;
    Palette:ARRAY OF TRGBQUAD;
    BitmapDatei:TBeRoStream;
    BitmapLaenge,PaletteLaenge,BytesGelesen:LONGWORD;
    Breite,Hoehe:INTEGER;
    pDaten:POINTER;
BEGIN
 RESULT:=FALSE;

 BitmapDatei:=TBeRoStream.Create;
 IF NOT DataPool.GetFile(Dateiname,BitmapDatei) THEN BEGIN
  BitmapDatei.Free;
  EXIT;
 END;
 BitmapDatei.Read(DateiHeader,SIZEOF(DateiHeader));
 BitmapDatei.Read(InfoHeader,SIZEOF(InfoHeader));

 PaletteLaenge:=InfoHeader.biClrUsed;
 SETLENGTH(Palette,PaletteLaenge);
 BytesGelesen:=BitmapDatei.Read(Palette,PaletteLaenge);
 IF BytesGelesen<>PaletteLaenge THEN BEGIN
  BitmapDatei.Free;
  EXIT;
 END;

 Breite:=InfoHeader.biWidth;
 Hoehe:=InfoHeader.biHeight;
 BitmapLaenge:=InfoHeader.biSizeImage;
 IF BitmapLaenge=0 THEN BitmapLaenge:=Breite*Hoehe*InfoHeader.biBitCount DIV 8;

 GETMEM(pDaten,BitmapLaenge);
 BytesGelesen:=BitmapDatei.Read(pDaten^,BitmapLaenge);
 IF BytesGelesen<>BitmapLaenge THEN BEGIN
  BitmapDatei.Free;
  EXIT;
 END;
 BitmapDatei.Free;

 SwapRGB(pDaten,Breite*Hoehe);

 Textur:=CreateTexture(Breite,Hoehe,GL_RGB,pDaten);
 FREEMEM(pDaten);
 RESULT:=TRUE;
END;

FUNCTION LoadTGATexture(Dateiname:STRING;VAR Texture:GLuint):BOOLEAN;
TYPE PBYTE=^BYTE;
     PLONGWORD=^LONGWORD;
     PLONGWORDS=^TLOMGWORDS;
     TLOMGWORDS=ARRAY[0..65536] OF LONGWORD;
     TTGAHeader=PACKED RECORD
      ImageID:BYTE;
      ColorMapType:BYTE;
      ImageType:BYTE;
      CMapSpec:PACKED RECORD
       FirstEntryIndex:WORD;
       Length:WORD;
       EntrySize:BYTE;
      END;
      OrigX:ARRAY[0..1] OF BYTE;
      OrigY:ARRAY[0..1] OF BYTE;
      Width:ARRAY[0..1] OF BYTE;
      Height:ARRAY[0..1] OF BYTE;
      BPP:BYTE;
      ImageInfo:BYTE;
     END;
TYPE TBGR=PACKED RECORD
      B,G,R:BYTE;
     END;
     TBGRA=PACKED RECORD
      B,G,R,A:BYTE;
     END;
     TRGBA=PACKED RECORD
      R,G,B,A:BYTE;
     END;
VAR TGAHeader:TTGAHeader;
    TGAFile:TBeRoStream;
    ImagePointer,NewImagePointer,Pixel:PLONGWORD;
    Width,Height,ImageSize,PixelCounter,I,L,J:LONGWORD;
    BGR:TBGR;
    BGRA:TBGRA;
    B,B1,B8:BYTE;
    W:WORD;
    HasPalette:BOOLEAN;
    Palette:ARRAY OF BYTE;
 FUNCTION RGBEncode:LONGWORD;
 VAR R:TRGBA;
 BEGIN
  R.R:=BGR.R;
  R.G:=BGR.G;
  R.B:=BGR.B;
  IF TGAHeader.ImageType=3 THEN BEGIN
   R.A:=(R.R+R.G+R.B) DIV 3;
  END ELSE BEGIN
   R.A:=255;
  END;
  RESULT:=(R.A SHL 24) OR (R.B SHL 16) OR (R.G SHL 8) OR R.R;
 END;
 FUNCTION RGBAEncode:LONGWORD;
 VAR R:TRGBA;
 BEGIN
  R.R:=BGRA.R;
  R.G:=BGRA.G;
  R.B:=BGRA.B;
  R.A:=BGRA.A;
  RESULT:=(R.A SHL 24) OR (R.B SHL 16) OR (R.G SHL 8) OR R.R;
 END;
 FUNCTION PaletteEncode(Index:LONGWORD):LONGWORD;
 VAR R:TRGBA;
     L:LONGWORD ABSOLUTE R;
     Offset:LONGWORD;
     W:WORD;
 BEGIN
  L:=0;
  IF (B8+TGAHeader.CMapSpec.FirstEntryIndex)<TGAHeader.CMapSpec.Length THEN BEGIN
   Offset:=Index*(TGAHeader.CMapSpec.EntrySize DIV 8);
   CASE TGAHeader.CMapSpec.EntrySize OF
    8:L:=Palette[Offset];
    16:BEGIN
     W:=Palette[Offset] OR (Palette[Offset+1] SHL 8);
     L:=(((W AND $8000) SHL 16) OR ((W AND $7C00) SHL 9) OR ((W AND $3E0) SHL 6) OR ((W AND $1F) SHL 3)) OR $0F0F0F0F;
    END;
    24:BEGIN
     R.R:=Palette[Offset+2];
     R.G:=Palette[Offset+1];
     R.B:=Palette[Offset];
     IF TGAHeader.ImageType=3 THEN BEGIN
      R.A:=(R.R+R.G+R.B) DIV 3;
     END ELSE BEGIN
      R.A:=255;
     END;
    END;
    32:BEGIN
     R.R:=Palette[Offset+3];
     R.G:=Palette[Offset+2];
     R.B:=Palette[Offset+1];
     R.A:=Palette[Offset];
    END;
   END;
  END;
  RESULT:=(R.A SHL 24) OR (R.B SHL 16) OR (R.G SHL 8) OR R.R;
 END;
 PROCEDURE FlipAndCorrectImage;
 VAR X,Y,O:LONGWORD;
     Line,NewLine:PLONGWORDS;
     Src,Dest:PLONGWORD;
 BEGIN
  IF (Width<>0) AND (Height<>0) THEN BEGIN
   IF (TGAHeader.ImageInfo AND $10)<>0 THEN BEGIN
    GETMEM(NewImagePointer,ImageSize);
    FOR Y:=0 TO Height-1 DO BEGIN
     O:=Y*Width*SIZEOF(LONGWORD);
     Line:=PLONGWORDS(LONGWORD(LONGWORD(ImagePointer)+O));
     NewLine:=PLONGWORDS(LONGWORD(LONGWORD(NewImagePointer)+O));
     FOR X:=0 TO Width-1 DO NewLine^[Width-(X+1)]:=Line^[X];
    END;
    FREEMEM(ImagePointer);
    ImagePointer:=NewImagePointer;
   END;
   IF (TGAHeader.ImageInfo AND $20)<>0 THEN BEGIN
    GETMEM(NewImagePointer,ImageSize);
    FOR Y:=0 TO Height-1 DO BEGIN
     Src:=PLONGWORD(LONGWORD(LONGWORD(ImagePointer)+(Y*Width*SIZEOF(LONGWORD))));
     Dest:=PLONGWORD(LONGWORD(LONGWORD(NewImagePointer)+((Height-(Y+1))*Width*SIZEOF(LONGWORD))));
     MOVE(Src^,Dest^,Width*SIZEOF(lONGWORD));
    END;
    FREEMEM(ImagePointer);
    ImagePointer:=NewImagePointer;
   END;
  END;
 END;
BEGIN
 RESULT:=FALSE;
 TGAFile:=TBeRoStream.Create;
 IF DataPool.GetFile(Dateiname,TGAFile) THEN BEGIN
  TGAFile.Seek(0);
  TGAFile.Read(TGAHeader,SIZEOF(TGAHeader));
  RESULT:=TRUE;
 END ELSE BEGIN
  TGAFile.Free;
  EXIT;
 END;
 IF RESULT THEN BEGIN
  TGAFile.Seek(0,bsoFromBeginning);
  TGAFile.Read(TGAHeader,SIZEOF(TGAHeader));
  TGAFile.Seek(TGAHeader.ImageID,bsoFromCurrent);
  Palette:=NIL;
  HasPalette:=TGAHeader.ColorMapType=1;
  IF HasPalette THEN BEGIN
   SETLENGTH(Palette,TGAHeader.CMapSpec.Length*TGAHeader.CMapSpec.EntrySize DIV 8);
   TGAFile.Read(Palette[0],LENGTH(Palette));
  END;
  IF HasPalette AND NOT (TGAHeader.CMapSpec.EntrySize IN [8,16,24,32]) THEN BEGIN
   RESULT:=FALSE;
   TGAFile.Free;
   EXIT;
  END;
  Width:=(TGAHeader.Width[1] SHL 8) OR TGAHeader.Width[0];
  Height:=(TGAHeader.Height[1] SHL 8) OR TGAHeader.Height[0];
  IF TGAHeader.ImageType IN [1,2,3] THEN BEGIN
   ImageSize:=(Width*Height)*SIZEOF(TBGRA);
   GETMEM(ImagePointer,ImageSize);
   Pixel:=ImagePointer;
   IF TGAHeader.BPP=8 THEN BEGIN
    IF (Width*Height)>0 THEN BEGIN
     CASE TGAHeader.ImageType OF
      1:BEGIN
       FOR I:=0 TO Width*Height-1 DO BEGIN
        TGAFile.Read(B8,SIZEOF(BYTE));
        Pixel^:=PaletteEncode(B8);
        INC(Pixel);
       END;
      END;
      2:BEGIN
       FOR I:=0 TO Width*Height-1 DO BEGIN
        TGAFile.Read(B8,SIZEOF(BYTE));
        Pixel^:=B8;
        INC(Pixel);
       END;
      END;
      3:BEGIN
       FOR I:=0 TO Width*Height-1 DO BEGIN
        TGAFile.Read(B8,SIZEOF(BYTE));
        BGR.B:=B8;
        BGR.G:=B8;
        BGR.R:=B8;
        Pixel^:=RGBEncode;
        INC(Pixel);
       END;
      END;
     END;
    END;
   END ELSE IF TGAHeader.BPP=16 THEN BEGIN
    IF (Width*Height)>0 THEN BEGIN
     FOR I:=0 TO Width*Height-1 DO BEGIN
      TGAFile.Read(W,SIZEOF(WORD));
      Pixel^:=(((W AND $8000) SHL 16) OR ((W AND $7C00) SHL 9) OR ((W AND $3E0) SHL 6) OR ((W AND $1F) SHL 3)) OR $0F0F0F0F;
      INC(Pixel);
     END;
    END;
   END ELSE IF TGAHeader.BPP=24 THEN BEGIN
    TGAFile.Read(ImagePointer^,ImageSize);
    IF (Width*Height)>0 THEN BEGIN
     FOR I:=0 TO Width*Height-1 DO BEGIN
      TGAFile.Read(BGR,SIZEOF(TBGR));
      Pixel^:=RGBEncode;
      INC(Pixel);
     END;
    END;
   END ELSE IF TGAHeader.BPP=32 THEN BEGIN
    IF (Width*Height)>0 THEN BEGIN
     FOR I:=0 TO Width*Height-1 DO BEGIN
      TGAFile.Read(BGRA,SIZEOF(TBGRA));
      Pixel^:=RGBAEncode;
      INC(Pixel);
     END;
    END;
   END;
   FlipAndCorrectImage;
   Texture:=CreateTexture(Width,Height,GL_RGBA,ImagePointer);
  END ELSE IF TGAHeader.ImageType IN [9,10,11] THEN BEGIN
   ImageSize:=(Width*Height)*SIZEOF(TBGRA);
   GETMEM(ImagePointer,ImageSize);
   Pixel:=ImagePointer;
   PixelCounter:=0;
   J:=Width*Height;
   IF TGAHeader.BPP=8 THEN BEGIN
    WHILE PixelCounter<J DO BEGIN
     TGAFile.Read(B1,SIZEOF(BYTE));
     B:=(B1 AND $7F)+1;
     IF (B1 AND $80)<>0 THEN BEGIN
      TGAFile.Read(B8,SIZEOF(BYTE));
      CASE TGAHeader.ImageType OF
       9:L:=PaletteEncode(B8);
       10:L:=B8;
       11:BEGIN
        BGR.B:=B8;
        BGR.G:=B8;
        BGR.R:=B8;
        L:=RGBEncode;
       END;
       ELSE L:=0;
      END;
      I:=0;
      WHILE I<B DO BEGIN
       Pixel^:=L;
       INC(Pixel);
       INC(PixelCounter);
       INC(I);
      END;
     END ELSE BEGIN
      I:=0;
      WHILE I<B DO BEGIN
       TGAFile.Read(B8,SIZEOF(BYTE));
       CASE TGAHeader.ImageType OF
        9:L:=PaletteEncode(B8);
        10:L:=B8;
        11:BEGIN
         BGR.B:=B8;
         BGR.G:=B8;
         BGR.R:=B8;
         L:=RGBEncode;
        END;
        ELSE L:=0;
       END;
       Pixel^:=L;
       INC(Pixel);
       INC(PixelCounter);
       INC(I);
      END;
     END;
    END;
   END ELSE IF TGAHeader.BPP=16 THEN BEGIN
    WHILE PixelCounter<J DO BEGIN
     TGAFile.Read(B1,SIZEOF(BYTE));
     B:=(B1 AND $7F)+1;
     IF (B1 AND $80)<>0 THEN BEGIN
      TGAFile.Read(W,SIZEOF(WORD));
      L:=(((W AND $8000) SHL 16) OR ((W AND $7C00) SHL 9) OR ((W AND $3E0) SHL 6) OR ((W AND $1F) SHL 3)) OR $0F0F0F0F;
      I:=0;
      WHILE I<B DO BEGIN
       Pixel^:=L;
       INC(Pixel);
       INC(PixelCounter);
       INC(I);
      END;
     END ELSE BEGIN
      I:=0;
      WHILE I<B DO BEGIN
       TGAFile.Read(W,SIZEOF(WORD));
       Pixel^:=(((W AND $8000) SHL 16) OR ((W AND $7C00) SHL 9) OR ((W AND $3E0) SHL 6) OR ((W AND $1F) SHL 3)) OR $0F0F0F0F;
       INC(Pixel);
       INC(PixelCounter);
       INC(I);
      END;
     END;
    END;
   END ELSE IF TGAHeader.BPP=24 THEN BEGIN
    WHILE PixelCounter<J DO BEGIN
     TGAFile.Read(B1,SIZEOF(BYTE));
     B:=(B1 AND $7F)+1;
     IF (B1 AND $80)<>0 THEN BEGIN
      TGAFile.Read(BGR,SIZEOF(TBGR));
      L:=RGBEncode;
      I:=0;
      WHILE I<B DO BEGIN
       Pixel^:=L;
       INC(Pixel);
       INC(PixelCounter);
       INC(I);
      END;
     END ELSE BEGIN
      I:=0;
      WHILE I<B DO BEGIN
       TGAFile.Read(BGR,SIZEOF(TBGR));
       Pixel^:=RGBEncode;
       INC(Pixel);
       INC(PixelCounter);
       INC(I);
      END;
     END;
    END;
   END ELSE IF TGAHeader.BPP=32 THEN BEGIN
    WHILE PixelCounter<J DO BEGIN
     TGAFile.Read(B1,SIZEOF(BYTE));
     B:=(B1 AND $7F)+1;
     IF (B1 AND $80)<>0 THEN BEGIN
      TGAFile.Read(BGRA,SIZEOF(TBGRA));
      L:=RGBAEncode;
      I:=0;
      WHILE I<B DO BEGIN
       Pixel^:=L;
       INC(Pixel);
       INC(PixelCounter);
       INC(I);
      END;
     END ELSE BEGIN
      I:=0;
      WHILE I<B DO BEGIN
       TGAFile.Read(BGRA,SIZEOF(TBGRA));
       Pixel^:=RGBAEncode;
       INC(Pixel);
       INC(PixelCounter);
       INC(I);
      END;
     END;
    END;
   END;
   FlipAndCorrectImage;
   Texture:=CreateTexture(Width,Height,GL_RGBA,ImagePointer);
  END;
  SETLENGTH(Palette,0);
  TGAFile.Free;
  RESULT:=TRUE;
 END;
END;

FUNCTION LoadFTXTexture(Dateiname:STRING;VAR Textur:GLuint):BOOLEAN;
VAR BitmapDatei:TBeRoStream;
    BitmapLaenge,BytesGelesen,Breite,Hoehe,Frames:LONGWORD;
    pDaten:POINTER;
BEGIN
 RESULT:=FALSE;

 BitmapDatei:=TBeRoStream.Create;
 IF NOT DataPool.GetFile(Dateiname,BitmapDatei) THEN BEGIN
  BitmapDatei.Free;
  EXIT;
 END;
 BitmapDatei.Read(Breite,SIZEOF(WORD));
 BitmapDatei.Read(Hoehe,SIZEOF(WORD));
 BitmapDatei.Read(Frames,SIZEOF(WORD));

 BitmapLaenge:=(Breite*Hoehe) SHL 2;

 GETMEM(pDaten,BitmapLaenge);
 BytesGelesen:=BitmapDatei.Read(pDaten^,BitmapLaenge);
 IF BytesGelesen<>BitmapLaenge THEN BEGIN
  BitmapDatei.Free;
  EXIT;
 END;
 BitmapDatei.Free;

 Textur:=CreateTexture(Breite,Hoehe,GL_RGBA,pDaten);
 FREEMEM(pDaten);
 RESULT:=TRUE;
END;

FUNCTION LoadTexture(Dateiname:STRING;VAR Textur:GLuint):BOOLEAN;
var Ext:string;
BEGIN
 RESULT:=FALSE;
 Dateiname:=lowercase(Dateiname);         
 Ext:=COPY(Dateiname,LENGTH(Dateiname)-3,4);
 if Ext='.bmp' then begin
  RESULT:=LoadBMPTexture(Dateiname,Textur);
 end else if Ext='.tga' then begin
  RESULT:=LoadTGATexture(Dateiname,Textur);
 end else if Ext='.ftx' then begin
  RESULT:=LoadFTXTexture(Dateiname,Textur);
 end;
END;

END.

