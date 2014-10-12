unit BeRoVectorCanvas;
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
{$endif}
{$R-,Q-}
{$assertions off}

interface

uses Math;

const pixelbits=8;
      pixelfactor=1 shl pixelbits;
      halfpixel=1 shl (pixelbits-1);
      onepixel=1 shl pixelbits;
      pixelmask=(1 shl pixelbits)-1;

      memoryincbits=8;
      memoryinc=1 shl memoryincbits;
      memoryincmask=memoryinc-1;

      PID_AppleUnicode=0;
      PID_Macintosh=1;
      PID_ISO=2;
      PID_Microsoft=3;

      SID_MAC_Roman=0;
      SID_MAC_Japanese=1;
      SID_MAC_Chinese=2;
      SID_MAC_Korean=3;
      SID_MAC_Arabic=4;
      SID_MAC_Hebrew=5;
      SID_MAC_Greek=6;
      SID_MAC_Russian=7;
      SID_MAC_RSymbol=8;
      SID_MAC_Devanagari=9;
      SID_MAC_Gurmukhi=10;
      SID_MAC_Gujarati=11;
      SID_MAC_Oriya=12;
      SID_MAC_Bengali=13;
      SID_MAC_Tamil=14;
      SID_MAC_Telugu=15;
      SID_MAC_Kannada=16;
      SID_MAC_Malayalam=17;
      SID_MAC_Sinhalese=18;
      SID_MAC_Burmese=19;
      SID_MAC_Khmer=20;
      SID_MAC_Thai=21;
      SID_MAC_Laotian=22;
      SID_MAC_Georgian=23;
      SID_MAC_Armenian=24;
      SID_MAC_Maldivian=25;
      SID_MAC_Tibetian=26;
      SID_MAC_Mongolian=27;
      SID_MAC_Geez=28;
      SID_MAC_Slavic=29;
      SID_MAC_Vietnamese=30;
      SID_MAC_Sindhi=31;
      SID_MAC_Uninterp=32;

      SID_MS_Undefined=0;
      SID_MS_UGL=1;

      SID_ISO_ASCII=0;
      SID_ISO_10646=1;
      SID_ISO_8859_1=2;

      LID_MS_Arabic=$0401;
      LID_MS_Bulgarian=$0402;
      LID_MS_Catalan=$0403;
      LID_MS_TraditionalChinese=$0404;
      LID_MS_SimplifiedChinese=$0804;
      LID_MS_Czech=$0405;
      LID_MS_Danish=$0406;
      LID_MS_German=$0407;
      LID_MS_SwissGerman=$0807;
      LID_MS_Greek=$0408;
      LID_MS_USEnglish=$0409;
      LID_MS_UKEnglish=$0809;
      LID_MS_CastilianSpanish=$040a;
      LID_MS_MexicanSpanish=$080a;
      LID_MS_ModernSpanish=$0c0a;
      LID_MS_Finnish=$040b;
      LID_MS_French=$040c;
      LID_MS_BelgianFrench=$080c;
      LID_MS_CanadianFrench=$0c0c;
      LID_MS_SwissFrench=$100c;
      LID_MS_Hebrew=$040d;
      LID_MS_Hungarian=$040e;
      LID_MS_Icelandic=$040f;
      LID_MS_Italian=$0410;
      LID_MS_SwissItalian=$0810;
      LID_MS_Japanese=$0411;
      LID_MS_Korean=$0412;
      LID_MS_Dutch=$0413;
      LID_MS_BelgianDutch=$0813;
      LID_MS_NorwegianBokmal=$0414;
      LID_MS_NorwegianNynorsk=$0814;
      LID_MS_Polish=$0415;
      LID_MS_BrazilianPortuguese=$0416;
      LID_MS_Portuguese=$0816;
      LID_MS_RhaetoRomanic=$0417;
      LID_MS_Romanian=$0418;
      LID_MS_Russian=$0419;
      LID_MS_CroatoSerbian=$041a;
      LID_MS_SerboCroatian=$081a;
      LID_MS_Slovakian=$041b;
      LID_MS_Albanian=$041c;
      LID_MS_Swedish=$041d;
      LID_MS_Thai=$041e;
      LID_MS_Turkish=$041f;
      LID_MS_Urdu=$0420;
      LID_MS_Bahasa=$0421;

      LID_MAC_English=0;
      LID_MAC_French=1;
      LID_MAC_German=2;
      LID_MAC_Italian=3;
      LID_MAC_Dutch=4;
      LID_MAC_Swedish=5;
      LID_MAC_Spanish=6;
      LID_MAC_Danish=7;
      LID_MAC_Portuguese=8;
      LID_MAC_Norwegian=9;
      LID_MAC_Hebrew=10;
      LID_MAC_Japanese=11;
      LID_MAC_Arabic=12;
      LID_MAC_Finnish=13;
      LID_MAC_Greek=14;
      LID_MAC_Icelandic=15;
      LID_MAC_Maltese=16;
      LID_MAC_Turkish=17;
      LID_MAC_Yugoslavian=18;
      LID_MAC_Chinese=19;
      LID_MAC_Urdu=20;
      LID_MAC_Hindi=21;
      LID_MAC_Thai=22;

      bvcfgfNONE=0;
      bvcfgfXY=1;
      bvcfgfX=2;
      bvcfgfY=3;

type TBeRoVectorCanvasRenderingMode=(bvcrmDRAFT,bvcrmCOLORINTENSITY,bvcrmRGB,bvcrmBGR);
     TBeRoVectorCanvasStyleMode=(bvcsmFILL,bvcsmLINE);
     TBeRoVectorCanvasLineCapMode=(bvclcmBUTT,bvclcmSQUARE,bvclcmROUND);
     TBeRoVectorCanvasLineJoinMode=(bvcljmBEVEL,bvcljmROUND,bvcljmMITER,bvcljmMITERREVERT,bvcljmMITERROUND);
     TBeRoVectorCanvasLineInnerJoinMode=(bvclijmBEVEL,bvclijmMITER,bvclijmJAG,bvclijmROUND);

     PBeRoVectorCanvasPoint=^TBeRoVectorCanvasPoint;
     TBeRoVectorCanvasPoint=record
      x,y,a,b,c,d,e,f:integer;
     end;
     TBeRoVectorCanvasPoints=array of TBeRoVectorCanvasPoint;
     TBeRoVectorCanvasPolygonPoints=array of TBeRoVectorCanvasPoints;

     PBeRoVectorCanvasCell=^TBeRoVectorCanvasCell;
     TBeRoVectorCanvasCell=record
      x,area,cover:integer;
      next:PBeRoVectorCanvasCell;
     end;
     TBeRoVectorCanvasCells=array of PBeRoVectorCanvasCell;

     PBeRoVectorCanvasSpan=^TBeRoVectorCanvasSpan;
     TBeRoVectorCanvasSpan=record
      x,len,coverage:integer;
     end;
     TBeRoVectorCanvasSpans=array of PBeRoVectorCanvasSpan;
 
     TBeRoVectorCanvasCustomColorHook=function(Instance:pointer;x,y,a,b,c,d,e,f:integer):longword;

     PBeRoVectorCanvasUDDCell=^TBeRoVectorCanvasUDDCell;
     TBeRoVectorCanvasUDDCell=record
      x,a,b,c,d,e,f:integer;
     end;
     TBeRoVectorCanvasUDDCells=array of PBeRoVectorCanvasUDDCell;

     TBeRoVectorCanvasScanLine=record
      cells:TBeRoVectorCanvasCells;
      numcells:integer;
      spans:TBeRoVectorCanvasSpans;
      numspans:integer;
      UDDcells:TBeRoVectorCanvasUDDCells;
      numUDDcells:integer;
     end;
     TBeRoVectorCanvasScanLines=array of TBeRoVectorCanvasScanLine;

     TBeRoVectorCanvasLinePoint=record
      x,y,d,UDDa,UDDb,UDDc,UDDd,UDDe,UDDf:integer;
     end;
     TBeRoVectorCanvasLinePoints=array of TBeRoVectorCanvasLinePoint;

     TBeRoVectorCanvasGammaLookUpTable=array[byte] of byte;

     TBeRoVectorCanvasColor=longword;

     TBeRoVectorCanvasMatrix=array[0..5] of single;

     TBeRoVectorCanvasUDDLine=record
      x1,x2,y1,y2,a1,a2,b1,b2,c1,c2,d1,d2,e1,e2,f1,f2:integer;
     end;
     TBeRoVectorCanvasUDDLines=array of TBeRoVectorCanvasUDDLine;

     TBeRoVectorCanvasCommandType=(bvcctMOVETO,bvcctLINETO);

     TBeRoVectorCanvasCommand=record
      CommandType:TBeRoVectorCanvasCommandType;
      x,y,a,b,c,d,e,f:integer;
     end;
     TBeRoVectorCanvasCommands=array of TBeRoVectorCanvasCommand;

     TBeRoVectorCanvasPolyPoint=record
      x,y:integer;
     end;
     TBeRoVectorCanvasPolyPoints=array of record
      Points:array of TBeRoVectorCanvasPolyPoint;
      NumPoints:integer;
     end;

     PBeRoVectorCanvasDraftEdge=^TBeRoVectorCanvasDraftEdge;
     TBeRoVectorCanvasDraftEdge=record
      fromx,fromy,tox,toy,dx,dy,absdx,absdy,x,y,error,errormax,xdir,ydir,erroracc,erroradj:integer;
      xmajor:boolean;
      xcoord:array[0..1] of integer;
      next:PBeRoVectorCanvasDraftEdge;
     end;
     TBeRoVectorCanvasDraftEdges=array of PBeRoVectorCanvasDraftEdge;

     TBeRoVectorCanvasShape=class
      private
       Commands:TBeRoVectorCanvasCommands;
       NumCommands:integer;
       cells:TBeRoVectorCanvasCells;
       numcells:integer;
       spans:TBeRoVectorCanvasSpans;
       numspans:integer;
       UDDcells:TBeRoVectorCanvasUDDCells;
       numUDDcells:integer;
       UDDLines:TBeRoVectorCanvasUDDLines;
       NumUDDLines:integer;
       scanlines:TBeRoVectorCanvasScanLines;
       CurrentGamma:single;
       CurrentWinding:boolean;
       CurrentColor:TBeRoVectorCanvasColor;
       CurrentHandleUDD:boolean;
       ClearUDDBuffer:boolean;
       UDDExtraPixels:integer;
       CustomColorProc:TBeRoVectorCanvasCustomColorHook;
       CustomColorInstance:pointer;
       CurrentWidth:integer;
       CurrentHeight:integer;
       CurrentRenderingMode:TBeRoVectorCanvasRenderingMode;
       CurrentMatrix:TBeRoVectorCanvasMatrix;
      public
       constructor Create;
       destructor Destroy; override;
     end;

     TBeRoVectorCanvas=class
      private
       GammaLookUpTable:TBeRoVectorCanvasGammaLookUpTable;
       PolyPoints:TBeRoVectorCanvasPolyPoints;
       NumPolyPoints:integer;
       DraftEdges:TBeRoVectorCanvasDraftEdges;
       ex,ey:integer;
       area,cover:integer;
       lastcx,lastcy,lastx,lasty,mx,my,ma,mb,mc,md,me,mf,lx,ly,lasta,lastb,lastc,lastd,laste,lastf:integer;
       CurrentWidth,CurrentHeight,CurrentWidthEx:integer;
       BufferCanvasUDD,BufferCanvas:pointer;
       renderminy,rendermaxy:integer;
       renderminmaxfirst:boolean;
       LinePoints:TBeRoVectorCanvasLinePoints;
       NumLinePoints:integer;
       IsExternalCanvas:boolean;
       NeedToClose:boolean;
       CurrentRenderingMode:TBeRoVectorCanvasRenderingMode;
       CurrentGamma:single;
       CurrentWinding:boolean;
       CurrentStyleMode:TBeRoVectorCanvasStyleMode;
       CurrentLineWidth:integer;
       CurrentLineCapMode:TBeRoVectorCanvasLineCapMode;
       CurrentLineJoinMode:TBeRoVectorCanvasLineJoinMode;
       CurrentLineInnerJoinMode:TBeRoVectorCanvasLineInnerJoinMode;
       CurrentColor:TBeRoVectorCanvasColor;
       CurrentHandleUDD:boolean;
       CurrentInvertedMatrix:TBeRoVectorCanvasMatrix;
       DefaultShape:TBeRoVectorCanvasShape;
       CurrentShape:TBeRoVectorCanvasShape;
       MatrixStack:array of TBeRoVectorCanvasMatrix;
       MatrixStackPosition:integer;
       ShapeStack:array of TBeRoVectorCanvasShape;
       ShapeStackPosition:integer;
       SaveCommands:boolean;
       FlushLineOnWork:boolean;
       procedure CheckScanLinesArray;
       function NewCell:PBeRoVectorCanvasCell;
       procedure RecordCell;
       procedure SetCell(nex,ney:integer;force:boolean=false);
       procedure StartCell(nex,ney:integer);
       function interpolateUDD(p,a1,a2,b1,b2:integer):integer;
       procedure RenderScanLine(ney,x1,y1,x2,y2:integer);
       procedure RenderLine(tox,toy:integer;toa:integer=0;tob:integer=0;toc:integer=0;tod:integer=0;toe:integer=0;tof:integer=0);
       procedure RenderSpanDRAFT(y:integer;span:PBeRoVectorCanvasSpan);
       procedure RenderSpanCOLORINTENSITY(y:integer;span:PBeRoVectorCanvasSpan);
       procedure RenderSpanRGBBGR(y:integer;span:PBeRoVectorCanvasSpan);
       procedure AddSpan(x,y,area,acount:integer);
       procedure GetDraftMinMaxY;
       procedure MakeDraftSpans;
       procedure SortScanLineCells;
       procedure OptimizeScanLineCells;
       procedure MakeScanLineSpans;
       procedure SortScanLineSpans;
       procedure ClipScanLineSpans;
       procedure OptimizeScanLineSpans;
       procedure RenderScanLineSpans;
       procedure FilterSubpixelBufferCanvas;
       procedure TranslateRGBBufferCanvas;
       procedure TranslateBGRBufferCanvas;
       procedure FlushLine;
       procedure CloseEx;
       procedure ApplyMatrix(var x,y:integer);
       procedure MoveToEx(tox,toy,toa,tob,toc,tod,toe,tof:integer);
       procedure LineToEx(tox,toy,toa,tob,toc,tod,toe,tof:integer);
       procedure AddLinePoint(x,y,a,b,c,d,e,f:integer);
       procedure ConvertLineStorkeToPolygon;
       procedure SetWidth(NewWidth:integer);
       procedure SetHeight(NewHeight:integer);
       procedure SetRenderingMode(NewRenderingMode:TBeRoVectorCanvasRenderingMode);
       procedure SetColor(AColor:TBeRoVectorCanvasColor);
       procedure SetWinding(AWinding:boolean);
       procedure SetStyleMode(AStyleMode:TBeRoVectorCanvasStyleMode);
       procedure SetLineWidth(ALineWidth:integer);
       procedure SetLineCapMode(ALineCapMode:TBeRoVectorCanvasLineCapMode);
       procedure SetLineJoinMode(ALineJoinMode:TBeRoVectorCanvasLineJoinMode);
       procedure SetLineInnerJoinMode(ALineInnerJoinMode:TBeRoVectorCanvasLineInnerJoinMode);
       procedure SetGamma(AGamma:single);
       procedure SetHandleUDD(AHandleUDD:boolean);
       procedure DrawUDDBuffer;
      public
       CurrentCanvas:pointer;
       ClearUDDBuffer:boolean;
       UDDExtraPixels:integer;
       Matrix:TBeRoVectorCanvasMatrix;
       CustomColorProc:TBeRoVectorCanvasCustomColorHook;
       CustomColorInstance:pointer;
       LineStrokePattern:ansistring;
       LineStrokePatternStepSize:integer;
       LineStrokePatternNewLine:boolean;
       constructor Create;
       destructor Destroy; override;
       function Canvas:pointer;
       procedure UseExternalCanvas(DstCanvas:pointer);
       procedure ResetShape;
       procedure ResetLocal;
       procedure Reset;
       procedure Setup(NewWidth,NewHeight:integer;NewRenderingMode:TBeRoVectorCanvasRenderingMode);
       procedure Clear(AColor:longword=$ffffff);
       procedure MoveTo(tox,toy:integer;toa:integer=0;tob:integer=0;toc:integer=0;tod:integer=0;toe:integer=0;tof:integer=0);
       procedure LineTo(tox,toy:integer;toa:integer=0;tob:integer=0;toc:integer=0;tod:integer=0;toe:integer=0;tof:integer=0);
       procedure QuadraticCurveTo(cx,cy,ax,ay:integer;ca:integer=0;cb:integer=0;cc:integer=0;cd:integer=0;ce:integer=0;cf:integer=0;aa:integer=0;ab:integer=0;ac:integer=0;ad:integer=0;ae:integer=0;af:integer=0;tolerance:integer=2;maxlevel:integer=32);
       procedure CubicCurveTo(c1x,c1y,c2x,c2y,ax,ay:integer;c1a:integer=0;c1b:integer=0;c1c:integer=0;c1d:integer=0;c1e:integer=0;c1f:integer=0;c2a:integer=0;c2b:integer=0;c2c:integer=0;c2d:integer=0;c2e:integer=0;c2f:integer=0;aa:integer=0;ab:integer=0;ac:integer=0;ad:integer=0;ae:integer=0;af:integer=0;tolerance:integer=2;maxlevel:integer=32);
       procedure Ellipse(x,y,rx,ry:integer;steps:integer=180;a1:integer=0;b1:integer=0;c1:integer=0;d1:integer=0;e1:integer=0;f1:integer=0;a2:integer=0;b2:integer=0;c2:integer=0;d2:integer=0;e2:integer=0;f2:integer=0);
       procedure Rectangle(x1,y1,x2,y2:integer;a1:integer=0;b1:integer=0;c1:integer=0;d1:integer=0;e1:integer=0;f1:integer=0;a2:integer=0;b2:integer=0;c2:integer=0;d2:integer=0;e2:integer=0;f2:integer=0);
       procedure Polygon(const Points:array of TBeRoVectorCanvasPoint);
       procedure PolyPolygon(const PolygonPoints:array of TBeRoVectorCanvasPoints);
       procedure Close;
       procedure Draw;
       procedure GetOrgXY(var x,y:integer); overload;
       procedure GetOrgXY(x,y:integer;out outx,outy:single); overload;
       procedure ResetMatrix;
       procedure PushMatrix;
       procedure PopMatrix;
       procedure CreateShape;
       procedure DestroyShape;
       function PopShape:TBeRoVectorCanvasShape;
       procedure DrawShape(Shape:TBeRoVectorCanvasShape);
      published
       property Width:integer read CurrentWidth write SetWidth;
       property Height:integer read CurrentHeight write SetHeight;
       property RenderingMode:TBeRoVectorCanvasRenderingMode read CurrentRenderingMode write SetRenderingMode;
       property Color:TBeRoVectorCanvasColor read CurrentColor write SetColor;
       property Winding:boolean read CurrentWinding write SetWinding;
       property StyleMode:TBeRoVectorCanvasStyleMode read CurrentStyleMode write SetStyleMode;
       property LineWidth:integer read CurrentLineWidth write SetLineWidth;
       property LineCapMode:TBeRoVectorCanvasLineCapMode read CurrentLineCapMode write SetLineCapMode;
       property LineJoinMode:TBeRoVectorCanvasLineJoinMode read CurrentLineJoinMode write SetLineJoinMode;
       property LineInnerJoinMode:TBeRoVectorCanvasLineInnerJoinMode read CurrentLineInnerJoinMode write SetLineInnerJoinMode;
       property HandleUDD:boolean read CurrentHandleUDD write SetHandleUDD;
       property Gamma:single read CurrentGamma write SetGamma;
     end;

     TBeRoVectorCanvasFontStyle=(bvcfsTHIN,bvcfsBOLD,bvcfsITALIC);
     TBeRoVectorCanvasFontStyles=set of TBeRoVectorCanvasFontStyle;

     TBeRoVectorCanvasFontCharStylePolygonCommandType=(bvcfcspctMOVETO,bvcfcspctLINETO,bvcfcspctCURVETO);

     TBeRoVectorCanvasFontCharStylePolygonCommandPoints=array[0..1] of TBeRoVectorCanvasPoint;

     TBeRoVectorCanvasFontCharStylePolygonCommand=record
      CommandType:TBeRoVectorCanvasFontCharStylePolygonCommandType;
      Points:TBeRoVectorCanvasFontCharStylePolygonCommandPoints;
     end;
     TBeRoVectorCanvasFontCharStylePolygonCommands=array of TBeRoVectorCanvasFontCharStylePolygonCommand;

     TBeRoVectorCanvasFontCharStylePolygon=record
      Commands:TBeRoVectorCanvasFontCharStylePolygonCommands;
     end;

     TBeRoVectorCanvasFontCharStyle=record
      AdvanceWidth,AdvanceHeight,LeftSideBearing,RightSideBearing,TopSideBearing,BottomSideBearing,XMin,YMin,XMax,YMax:integer;
      Polygon:TBeRoVectorCanvasFontCharStylePolygon;
     end;

     TBeRoVectorCanvasFontGlyph=record
      AdvanceWidth,AdvanceHeight,LeftSideBearing,RightSideBearing,TopSideBearing,BottomSideBearing,XMin,YMin,XMax,YMax:integer;
      Styles:array[0..5] of TBeRoVectorCanvasFontCharStyle;
     end;
     TBeRoVectorCanvasFontGlyphs=array of TBeRoVectorCanvasFontGlyph;

     TBeRoVectorCanvasFontKerningPair=record
      Left,Right,Value:integer;
     end;
     TBeRoVectorCanvasFontKerningPairs=array of TBeRoVectorCanvasFontKerningPair;

     TBeRoVectorCanvasFontKerningTable=record
      Horizontal:boolean;
      ValueOverride:boolean;
      BinarySearch:boolean;
      KerningPairs:TBeRoVectorCanvasFontKerningPairs;
     end;
     TBeRoVectorCanvasFontKerningTables=array of TBeRoVectorCanvasFontKerningTable;

     PBeRoVectorCanvasFontCharacterMapSub=^TBeRoVectorCanvasFontCharacterMapSub;
     TBeRoVectorCanvasFontCharacterMapSub=array[0..$ff] of word;

     TBeRoVectorCanvasFontCharacterMap=array[0..$10ff] of PBeRoVectorCanvasFontCharacterMapSub;

     TBeRoVectorCanvasFont=class
      private
       Glyphs:TBeRoVectorCanvasFontGlyphs;
       KerningTables:TBeRoVectorCanvasFontKerningTables;
       CharacterMap:TBeRoVectorCanvasFontCharacterMap;
       AdvanceWidthMax,AdvanceHeightMax,UnitsPerEm,UnitsPerPixel,XMin,YMin,XMax,YMax:integer;
       function GetCharIndex(CharCode:longword):longword;
       function Kerning(Left,Right:integer;Horizontal:boolean):integer;
       function GetCharCode(const Text:widestring;var Index:integer):longword;
      public
       Name:widestring;
       Styles:TBeRoVectorCanvasFontStyles;
       Size:integer;
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       function LoadTrueType(var Data;Size:integer;PlatformID:integer=PID_Microsoft;SpecificID:integer=SID_MS_UGL;LanguageID:integer=LID_MS_USEnglish;CollectionIndex:integer=0):boolean; overload;
       function LoadTrueType(const FileName:ansistring;PlatformID:integer=PID_Microsoft;SpecificID:integer=SID_MS_UGL;LanguageID:integer=LID_MS_USEnglish;CollectionIndex:integer=0):boolean; overload;
       function TextWidth(const Text:widestring):integer;
       function TextHeight(const Text:widestring):integer;
       function RowHeight(const Percent:integer):integer;
       procedure Draw(Canvas:TBeRoVectorCanvas;x,y:integer;const Text:widestring;GridFitting:integer=bvcfgfNONE;GridFittingCharwise:boolean=false;tolerance:integer=2;maxlevel:integer=32);
     end;

const MatrixIdentity:TBeRoVectorCanvasMatrix=(1,0,0,1,0,0);
      MatrixNull:TBeRoVectorCanvasMatrix=(0,0,0,0,0,0);

function MatrixTranslate(tx,ty:single):TBeRoVectorCanvasMatrix;
function MatrixScale(sx,sy:single):TBeRoVectorCanvasMatrix;
function MatrixRotate(degress:single):TBeRoVectorCanvasMatrix;
function MatrixSkewX(x:single):TBeRoVectorCanvasMatrix;
function MatrixSkewY(y:single):TBeRoVectorCanvasMatrix;
function MatrixMul(const a,b:TBeRoVectorCanvasMatrix):TBeRoVectorCanvasMatrix;
function MatrixInverse(const a:TBeRoVectorCanvasMatrix):TBeRoVectorCanvasMatrix;
procedure ApplyMatrixToXY(const m:TBeRoVectorCanvasMatrix;var x,y:integer); overload;
procedure ApplyMatrixToXY(const m:TBeRoVectorCanvasMatrix;var x,y:single); overload;
function Point(x,y:integer;a:integer=0;b:integer=0;c:integer=0;d:integer=0;e:integer=0;f:integer=0):TBeRoVectorCanvasPoint;

implementation

const deg2rad=pi/180;
      rad2deg=180/pi;

      NID_Copyright=0;
      NID_Family=1;
      NID_Subfamily=2;
      NID_UniqueID=3;
      NID_FullName=4;
      NID_Version=5;
      NID_PostscriptName=6;
      NID_Trademark=7;

      CMAP_FORMAT0=0;
      CMAP_FORMAT2=2;
      CMAP_FORMAT4=4;
      CMAP_FORMAT6=6;
      CMAP_FORMAT8=8;
      CMAP_FORMAT10=10;
      CMAP_FORMAT12=12;
      CMAP_FORMAT13=13;

      OFFSET_TABLE_SIZE=12;

      MACINTOSH=1;
      MICROSOFT=2;

      TT_NO_POINT=0;
      TT_OFF_CURVE=1;
      TT_ON_CURVE=2;

      TT_ERR_NoError=0;
      TT_ERR_InvalidFile=1;
      TT_ERR_CorruptFile=2;
      TT_ERR_OutOfMemory=2;
      TT_ERR_TableNotFound=3;
      TT_ERR_NoCharacterMapFound=4;
      TT_ERR_UnknownCharacterMapFormat=5;
      TT_ERR_CharactermapNotPresent=6;
      TT_ERR_UnableToOpenFile=7;
      TT_ERR_UnknownKerningFormat=8;

type pbytearray=^tbytearray;
     tbytearray=array[0..65535] of byte;

     pbyte=^byte;
     pshortint=^shortint;
     pword=^word;
     psmallint=^smallint;
     plongword=^longword;
     plongint=^longint;
     pdouble=^double;

     TBeRoVectorCanvasTrueTypeKerningPair=packed record
      Left:word;
      Right:word;
      Value:smallint;
     end;
     TBeRoVectorCanvasTrueTypeKerningPairs=array of TBeRoVectorCanvasTrueTypeKerningPair;

     TBeRoVectorCanvasTrueTypeKerningTable=record
      Horizontal:boolean;
      ValueOverride:boolean;
      BinarySearch:boolean;
      KerningPairs:TBeRoVectorCanvasTrueTypeKerningPairs;
     end;
     TBeRoVectorCanvasTrueTypeKerningTables=array of TBeRoVectorCanvasTrueTypeKerningTable;

     TBeRoVectorCanvasTrueTypePoint=packed record
      X:integer;
      Y:integer;
      OnCurve:boolean;
     end;
     TBeRoVectorCanvasTrueTypePoints=array of TBeRoVectorCanvasTrueTypePoint;

     TBeRoVectorCanvasTrueTypeContour=record
      Points:TBeRoVectorCanvasTrueTypePoints;
     end;
     TBeRoVectorCanvasTrueTypeContours=array of TBeRoVectorCanvasTrueTypeContour;

     TBeRoVectorCanvasTrueTypeCompositeSubGlyph=record
      Glyph,Flags:word;
      x,y,xx,yx,xy,yy:integer;
     end;

     TBeRoVectorCanvasTrueTypeCompositeSubGlyphs=array of TBeRoVectorCanvasTrueTypeCompositeSubGlyph;

     PBeRoVectorCanvasTrueTypeGlyph=^TBeRoVectorCanvasTrueTypeGlyph;
     TBeRoVectorCanvasTrueTypeGlyph=record
      Contours:TBeRoVectorCanvasTrueTypeContours;
      Points:TBeRoVectorCanvasTrueTypePoints;
      CompositeSubGlyphs:TBeRoVectorCanvasTrueTypeCompositeSubGlyphs;
      XMin:smallint;
      YMin:smallint;
      XMax:smallint;
      YMax:smallint;
      AdvanceWidth:word;
      AdvanceHeight:word;
      LeftSideBearing:smallint;
      RightSideBearing:smallint;
      TopSideBearing:smallint;
      BottomSideBearing:smallint;
      UseMetricsFrom:integer;
     end;
     TBeRoVectorCanvasTrueTypeGlyphs=array of TBeRoVectorCanvasTrueTypeGlyph;

     TBeRoVectorCanvasTrueTypeBytes=array of byte;

     TBeRoVectorCanvasTrueTypeLongword=array of longword;

     TBeRoVectorCanvasTrueTypeFontGetGlyphIndexSubHeaderKeys=array[0..255] of word;

     TBeRoVectorCanvasTrueTypeFont=class
      private
       FontData:TBeRoVectorCanvasTrueTypeBytes;
       Glyphs:TBeRoVectorCanvasTrueTypeGlyphs;
       CMap:TBeRoVectorCanvasTrueTypeBytes;
       GlyphOffsetArray:TBeRoVectorCanvasTrueTypeLongword;
       KerningTables:TBeRoVectorCanvasTrueTypeKerningTables;

       subHeaderKeys:TBeRoVectorCanvasTrueTypeFontGetGlyphIndexSubHeaderKeys;

       FontDataSize:integer;

       numberOfHMetrics:word;
       numberOfVMetrics:word;
       numTables:word;
       cmapFormat:word;

       LastError:word;
       indexToLocFormat:smallint;
       strcopyright:widestring;
       strfamily:widestring;
       strsubfamily:widestring;
       strfullName:widestring;
       struniqueid:widestring;
       strversion:widestring;
       strpostscript:widestring;
       strtrademark:widestring;
       xMax:smallint;
       xMin:smallint;
       yMax:smallint;
       yMin:smallint;
       unitsPerEm:word;
       ascender:word;
       descender:word;
       lineGap:word;
       AdvanceWidthMax:word;
       AdvanceHeightMax:word;
       platformID:word;
       specificID:word;
       languageID:word;
       function ReadFontData(DataPointer:pointer;DataSize,CollectionIndex:integer):integer;
       function GetTableDirEntry(Tag:longword;var CheckSum,Offset,Size:longword):integer;
       function ProcessFontHeaderTable:integer;
       function ProcessMaximumProfileTable:integer;
       function ProcessNamingTable:integer;
       function ProcessIndexToLocationTable:integer;
       function ProcessGlyphDataTable:integer;
       function ProcessHorizontalHeaderTable:integer;
       function ProcessHorizontalMetricsTable:integer;
       function ProcessVerticalHeaderTable:integer;
       function ProcessVerticalMetricsTable:integer;
       function ProcessKerningTable:integer;
       function ProcessCharacterMappingTable:integer;
       function GetGlyphIndex(CharCode:longword):word;
      public
       constructor Create(DataPointer:pointer;DataSize:integer;pid,sid,lid:word;CollectionIndex:integer=0);
       destructor Destroy; override;
       function numGlyphs:integer;
       function Glyph(Code:longword):PBeRoVectorCanvasTrueTypeGlyph;
       function Kerning(Left,Right:word;Horizontal:boolean):smallint;
       function CharacterMap(c:longword):word; overload;
       function CharacterMap(c:widechar):word; overload;
       function NumContours(GlyphNum:word):smallint;
       function NumPoints(GlyphNum,ContourNum:word):smallint;
       function PointX(GlyphNum,ContourNum,PointNum:word):integer;
       function PointY(GlyphNum,ContourNum,PointNum:word):integer;
       function OnCurve(GlyphNum,ContourNum,PointNum:word):boolean;
       function GlyphXMin(GlyphNum:word):smallint;
       function GlyphYMin(GlyphNum:word):smallint;
       function GlyphXMax(GlyphNum:word):smallint;
       function GlyphYMax(GlyphNum:word):smallint;
     end;

function SoftTRUNC(FloatValue:single):integer;
begin
 result:=trunc(FloatValue);
end;
{
type plongword=^longword;
const MaskMantissa=(1 shl 23)-1;
var Exponent,Mantissa,Sig,SigExtra,Signed,IsDenormalized:longword;
    value,Shift:integer;
begin
 Exponent:=(plongword(@FloatValue)^ and $7ffffffF) shr 23;
 Mantissa:=plongword(@FloatValue)^ and MaskMantissa;
 Shift:=Exponent-$96;
 Sig:=Mantissa or $00800000;
 SigExtra:=Sig shl (Shift and 31);
 IsDenormalized:=0-ord(0<=Shift);
 Value:=(((-ord(Exponent>=$7E)) and (Sig shr (32-Shift))) and not IsDenormalized) or (SigExtra and IsDenormalized);
 Signed:=0-ord((plongword(@FloatValue)^ and $80000000)<>0);
 result:=(((0-Value) and Signed) or (Value and not Signed)) and (0-ord($9E>Exponent));
end;
 }
function calc_point_location(x1,y1,x2,y2,x3,y3:integer):integer;
begin
 result:=((x3-x2)*(y2-y1))-((y3-y2)*(x2-x1));
end;

(*
{$IFDEF cpu386}
function SoftSQRT(Value:single):single; stdcall;
asm
 sub dword ptr Value,$3f800000
 sar dword ptr Value,1
 add dword ptr Value,$3f800000
 fld dword ptr Value
end;

function arctan2(const y,x:single):single; assembler; register;
asm
 fld dword ptr y
 fld dword ptr x
 fpatan
 fwait
end;

function arccos(const x:single):single; assembler; register;
asm
 fld1
 fld dword ptr x
 fst st(2)
 fmul st(0),st(0)
 fsubp
 fsqrt
 fxch
 fpatan
end;

function pow(Number,Exponent:single):single; assembler; stdcall;
asm
 fld dword ptr Exponent
 fld dword ptr Number
 fyl2x
 fld1
 fld st(1)
 fprem
 f2xm1
 faddp st(1),st
 fscale
 fstp ST(1)
end;

{$ELSE}

function arctan(const x:single):single;
begin
 result:=0.05030176425872175099*(-6.9888366207752135+x)*(3.14559995508649281E-7+x)*((2.84446368839622429+0.826399783297673451*x)+(x*x))/(1+(0.1471039133652469065841349249*x)+(0.644464067689154755092299698*x*x));
end;

function arctan2(const y,x:single):single;
begin
 result:=arctan(y/x);
end;

function arccos(const y,x:single):single;
begin
 result:=arctan2(sqrt((1+x)*(1-x)),x);
end;

function pow(Number,Exponent:single):single;
begin
 result:=exp(Exponent*ln(Number));
end;

function SoftSQRT(Value:single):single;
begin
 longint(pointer(@result)^):=((longint(pointer(@Value)^)-$3F800000) div 2)+$3F800000;
end;
{$ENDIF}
*)

function pow(Number,Exponent:single):single;
begin
 result:=exp(Exponent*ln(Number));
end;

function SoftSQRT(Value:single):single;
begin
 result:=sqrt(Value);
end;

function tan(x:single):single;
begin
 result:=cos(x);
 if result<1E-20 then begin
  result:=0;
 end else begin
  result:=sin(x)/result;
 end;
end;

function sar(Value,Shift:integer):integer;
{$IFDEF CPU386} assembler; register;
{$ELSE}{$IFDEF FPC}inline;
{$ELSE} register;
{$ENDIF}{$ENDIF}
{$IFDEF CPU386}
asm
 mov ecx,edx
 sar eax,cl
end;
{$ELSE}
begin
 result:=(Value shr Shift) or (($ffffffff+(1-((Value and (1 shl 31)) shr 31) and ord(Shift<>0))) shl (32-Shift));
end;
{$ENDIF}

{function isqrt(x:longword):longword;
var t,s:longword;
begin
 result:=0;
 t:=$40000000;
 while t<>0 do begin
  s:=result+t;
  if s<=x then begin
   dec(x,s);
   result:=s+t;
  end;
  result:=result shr 1;
  t:=t shr 2;
 end;
end;}

function isqrt(x:longword):longword;
const isqrtLookUpTable:array[0..1023] of longword=(
       0,2048,2896,3547,4096,4579,5017,5418,5793,6144,6476,6792,7094,7384,7663,7932,8192,8444,
       8689,8927,9159,9385,9606,9822,10033,10240,10443,10642,10837,11029,11217,11403,11585,
       11765,11942,12116,12288,12457,12625,12790,12953,13114,13273,13430,13585,13738,13890,
       14040,14189,14336,14482,14626,14768,14910,15050,15188,15326,15462,15597,15731,15864,
       15995,16126,16255,16384,16512,16638,16764,16888,17012,17135,17257,17378,17498,17618,
       17736,17854,17971,18087,18203,18318,18432,18545,18658,18770,18882,18992,19102,19212,
       19321,19429,19537,19644,19750,19856,19961,20066,20170,20274,20377,20480,20582,20684,
       20785,20886,20986,21085,21185,21283,21382,21480,21577,21674,21771,21867,21962,22058,
       22153,22247,22341,22435,22528,22621,22713,22806,22897,22989,23080,23170,23261,23351,
       23440,23530,23619,23707,23796,23884,23971,24059,24146,24232,24319,24405,24491,24576,
       24661,24746,24831,24915,24999,25083,25166,25249,25332,25415,25497,25580,25661,25743,
       25824,25905,25986,26067,26147,26227,26307,26387,26466,26545,26624,26703,26781,26859,
       26937,27015,27092,27170,27247,27324,27400,27477,27553,27629,27705,27780,27856,27931,
       28006,28081,28155,28230,28304,28378,28452,28525,28599,28672,28745,28818,28891,28963,
       29035,29108,29180,29251,29323,29394,29466,29537,29608,29678,29749,29819,29890,29960,
       30030,30099,30169,30238,30308,30377,30446,30515,30583,30652,30720,30788,30856,30924,
       30992,31059,31127,31194,31261,31328,31395,31462,31529,31595,31661,31727,31794,31859,
       31925,31991,32056,32122,32187,32252,32317,32382,32446,32511,32575,32640,32704,32768,
       32832,32896,32959,33023,33086,33150,33213,33276,33339,33402,33465,33527,33590,33652,
       33714,33776,33839,33900,33962,34024,34086,34147,34208,34270,34331,34392,34453,34514,
       34574,34635,34695,34756,34816,34876,34936,34996,35056,35116,35176,35235,35295,35354,
       35413,35472,35531,35590,35649,35708,35767,35825,35884,35942,36001,36059,36117,36175,
       36233,36291,36348,36406,36464,36521,36578,36636,36693,36750,36807,36864,36921,36978,
       37034,37091,37147,37204,37260,37316,37372,37429,37485,37540,37596,37652,37708,37763,
       37819,37874,37929,37985,38040,38095,38150,38205,38260,38315,38369,38424,38478,38533,
       38587,38642,38696,38750,38804,38858,38912,38966,39020,39073,39127,39181,39234,39287,
       39341,39394,39447,39500,39553,39606,39659,39712,39765,39818,39870,39923,39975,40028,
       40080,40132,40185,40237,40289,40341,40393,40445,40497,40548,40600,40652,40703,40755,
       40806,40857,40909,40960,41011,41062,41113,41164,41215,41266,41317,41368,41418,41469,
       41519,41570,41620,41671,41721,41771,41821,41871,41922,41972,42021,42071,42121,42171,
       42221,42270,42320,42369,42419,42468,42518,42567,42616,42665,42714,42763,42813,42861,
       42910,42959,43008,43057,43105,43154,43203,43251,43300,43348,43396,43445,43493,43541,
       43589,43637,43685,43733,43781,43829,43877,43925,43972,44020,44068,44115,44163,44210,
       44258,44305,44352,44400,44447,44494,44541,44588,44635,44682,44729,44776,44823,44869,
       44916,44963,45009,45056,45103,45149,45195,45242,45288,45334,45381,45427,45473,45519,
       45565,45611,45657,45703,45749,45795,45840,45886,45932,45977,46023,46069,46114,46160,
       46205,46250,46296,46341,46386,46431,46477,46522,46567,46612,46657,46702,46746,46791,
       46836,46881,46926,46970,47015,47059,47104,47149,47193,47237,47282,47326,47370,47415,
       47459,47503,47547,47591,47635,47679,47723,47767,47811,47855,47899,47942,47986,48030,
       48074,48117,48161,48204,48248,48291,48335,48378,48421,48465,48508,48551,48594,48637,
       48680,48723,48766,48809,48852,48895,48938,48981,49024,49067,49109,49152,49195,49237,
       49280,49322,49365,49407,49450,49492,49535,49577,49619,49661,49704,49746,49788,49830,
       49872,49914,49956,49998,50040,50082,50124,50166,50207,50249,50291,50332,50374,50416,
       50457,50499,50540,50582,50623,50665,50706,50747,50789,50830,50871,50912,50954,50995,
       51036,51077,51118,51159,51200,51241,51282,51323,51364,51404,51445,51486,51527,51567,
       51608,51649,51689,51730,51770,51811,51851,51892,51932,51972,52013,52053,52093,52134,
       52174,52214,52254,52294,52334,52374,52414,52454,52494,52534,52574,52614,52654,52694,
       52734,52773,52813,52853,52892,52932,52972,53011,53051,53090,53130,53169,53209,53248,
       53287,53327,53366,53405,53445,53484,53523,53562,53601,53640,53679,53719,53758,53797,
       53836,53874,53913,53952,53991,54030,54069,54108,54146,54185,54224,54262,54301,54340,
       54378,54417,54455,54494,54532,54571,54609,54647,54686,54724,54762,54801,54839,54877,
       54915,54954,54992,55030,55068,55106,55144,55182,55220,55258,55296,55334,55372,55410,
       55447,55485,55523,55561,55599,55636,55674,55712,55749,55787,55824,55862,55900,55937,
       55975,56012,56049,56087,56124,56162,56199,56236,56273,56311,56348,56385,56422,56459,
       56497,56534,56571,56608,56645,56682,56719,56756,56793,56830,56867,56903,56940,56977,
       57014,57051,57087,57124,57161,57198,57234,57271,57307,57344,57381,57417,57454,57490,
       57527,57563,57599,57636,57672,57709,57745,57781,57817,57854,57890,57926,57962,57999,
       58035,58071,58107,58143,58179,58215,58251,58287,58323,58359,58395,58431,58467,58503,
       58538,58574,58610,58646,58682,58717,58753,58789,58824,58860,58896,58931,58967,59002,
       59038,59073,59109,59144,59180,59215,59251,59286,59321,59357,59392,59427,59463,59498,
       59533,59568,59603,59639,59674,59709,59744,59779,59814,59849,59884,59919,59954,59989,
       60024,60059,60094,60129,60164,60199,60233,60268,60303,60338,60373,60407,60442,60477,
       60511,60546,60581,60615,60650,60684,60719,60753,60788,60822,60857,60891,60926,60960,
       60995,61029,61063,61098,61132,61166,61201,61235,61269,61303,61338,61372,61406,61440,
       61474,61508,61542,61576,61610,61644,61678,61712,61746,61780,61814,61848,61882,61916,
       61950,61984,62018,62051,62085,62119,62153,62186,62220,62254,62287,62321,62355,62388,
       62422,62456,62489,62523,62556,62590,62623,62657,62690,62724,62757,62790,62824,62857,
       62891,62924,62957,62991,63024,63057,63090,63124,63157,63190,63223,63256,63289,63323,
       63356,63389,63422,63455,63488,63521,63554,63587,63620,63653,63686,63719,63752,63785,
       63817,63850,63883,63916,63949,63982,64014,64047,64080,64113,64145,64178,64211,64243,
       64276,64309,64341,64374,64406,64439,64471,64504,64536,64569,64601,64634,64666,64699,
       64731,64763,64796,64828,64861,64893,64925,64957,64990,65022,65054,65086,65119,65151,
       65183,65215,65247,65279,65312,65344,65376,65408,65440,65472,65504);
      isqrtBitSliderLookUpTable:array[byte] of byte=(
       0,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
       5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
       6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
       6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
       7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
       7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
       7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
       7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7);
var bit:integer;
    t,shift:longword;
begin
 t:=x;
 shift:=11;
 bit:=t shr 24;
 if bit<>0 then begin
  bit:=isqrtBitSliderLookUpTable[bit]+24;
 end else begin
  bit:=(t shr 16) and $ff;
  if bit<>0 then begin
   bit:=isqrtBitSliderLookUpTable[bit]+16;
  end else begin
   bit:=(t shr 8) and $ff;
   if bit<>0 then begin
    bit:=isqrtBitSliderLookUpTable[bit]+8;
   end else begin
    bit:=isqrtBitSliderLookUpTable[t];
   end;
  end;
 end;
 bit:=bit-9;
 if bit>0 then begin
  bit:=(sar(bit,1))+(bit and 1);
  dec(shift,bit);
  x:=x shr (bit shl 1);
 end;
 result:=isqrtLookUpTable[x] shr shift;
end;

function VectorLength(x,y:integer):integer;
const Inv256=1/256;
begin
 if (abs(x)+abs(y))<32768 then begin
  result:=isqrt((x*x)+(y*y));
 end else begin
  result:=round(sqrt(sqr(x*Inv256)+sqr(y*Inv256))*256);
 end;
end;

function blend(c1,c2,alpha:longword):longword;
var v1,v2,t,a:longword;
begin
 v1:=c1 and $ff0000;
 v2:=c2 and $ff0000;
 t:=((((v2-v1)*alpha)+(v1*256)) div 256) and $ff0000;
 result:=t;

 v1:=c1 and $00ff00;
 v2:=c2 and $00ff00;
 t:=((((v2-v1)*alpha)+(v1*256)) div 256) and $00ff00;
 result:=result or t;

 v1:=c1 and $0000ff;
 v2:=c2 and $0000ff;
 t:=((((v2-v1)*alpha)+(v1*256)) div 256) and $0000ff;
 result:=result or t;

 a:=((Alpha+(c1 shr 24))-(((Alpha*(c1 shr 24))+255) shr 8)) and $ff;
 result:=result or (a shl 24);
end;

function MatrixTranslate(tx,ty:single):TBeRoVectorCanvasMatrix;
begin
 result:=MatrixIdentity;
 result[4]:=tx;
 result[5]:=ty;
end;

function MatrixScale(sx,sy:single):TBeRoVectorCanvasMatrix;
begin
 result:=MatrixIdentity;
 result[0]:=sx;
 result[3]:=sy;
end;

function MatrixRotate(degress:single):TBeRoVectorCanvasMatrix;
var rad,c,s:single;
begin
 rad:=degress*deg2rad;
 c:=cos(rad);
 s:=sin(rad);
 result:=MatrixIdentity;
 result[0]:=c;
 result[1]:=s;
 result[2]:=-s;
 result[3]:=c;
end;

function MatrixSkewX(x:single):TBeRoVectorCanvasMatrix;
begin
 result:=MatrixIdentity;
 result[1]:=tan(x*deg2rad);
end;

function MatrixSkewY(y:single):TBeRoVectorCanvasMatrix;
begin
 result:=MatrixIdentity;
 result[2]:=tan(y*deg2rad);
end;

function MatrixMul(const a,b:TBeRoVectorCanvasMatrix):TBeRoVectorCanvasMatrix;
begin
 result[0]:=(a[0]*b[0])+(a[1]*b[2]);
 result[1]:=(a[0]*b[1])+(a[1]*b[3]);
 result[2]:=(a[2]*b[0])+(a[3]*b[2]);
 result[3]:=(a[2]*b[1])+(a[3]*b[3]);
 result[4]:=(a[4]*b[0])+(a[5]*b[2])+b[4];
 result[5]:=(a[4]*b[1])+(a[5]*b[3])+b[5];
end;

function MatrixInverse(const a:TBeRoVectorCanvasMatrix):TBeRoVectorCanvasMatrix;
var det,idet:single;
begin
 det:=(a[0]*a[3])-(a[1]*a[2]);
 if abs(det)<1E-14 then begin
  result:=a;
 end else begin
  idet:=1/det;
  result[0]:=a[3]*idet;
  result[1]:=-a[1]*idet;
  result[2]:=-a[2]*idet;
  result[3]:=a[0]*idet;
  result[4]:=-(a[4]*result[0])-(a[5]*result[2]);
  result[5]:=-(a[4]*result[1])-(a[5]*result[3]);
 end;
end;

procedure ApplyMatrixToXY(const m:TBeRoVectorCanvasMatrix;var x,y:integer); overload;
var tx:integer;
begin
 tx:=x;
 x:=softtrunc((tx*m[0])+(y*m[2])+m[4]);
 y:=softtrunc((tx*m[1])+(y*m[3])+m[5]);
end;

procedure ApplyMatrixToXY(const m:TBeRoVectorCanvasMatrix;var x,y:single); overload;
var tx:single;
begin
 tx:=x;
 x:=(tx*m[0])+(y*m[2])+m[4];
 y:=(tx*m[1])+(y*m[3])+m[5];
end;

function Point(x,y:integer;a:integer=0;b:integer=0;c:integer=0;d:integer=0;e:integer=0;f:integer=0):TBeRoVectorCanvasPoint;
begin
 result.x:=x;
 result.x:=y;
 result.a:=a;
 result.b:=b;
 result.c:=c;
 result.d:=d;
 result.e:=e;
 result.f:=f;
end;

function toDOUBLE(x:longword):double;
var a:smallint;
    aw:word absolute a;
begin
 aw:=x div 65536;
 if a>0 then begin
  result:=aw+((x mod 65536)/$ffffffff);
 end else begin
  result:=aw-((x mod 65536)/$ffffffff);
 end;
end;

function toWORD(b1,b2:byte):word;
begin
 result:=(b1 shl 8) or b2;
end;

function toSMALLINT(b1,b2:byte):smallint;
begin
 result:=smallint(word(toWORD(b1,b2)));
end;

function toLONGWORD(b1,b2,b3,b4:byte):longword;
begin
 result:=(b1 shl 24) or (b2 shl 16) or (b3 shl 8) or b4;
end;

function IsBitSet(ByteValue,Bit:byte):boolean;
begin
 result:=(ByteValue and (1 shl Bit))<>0;
end;

constructor TBeRoVectorCanvasTrueTypeFont.Create(DataPointer:pointer;DataSize:integer;pid,sid,lid:word;CollectionIndex:integer=0);
begin
 inherited Create;
 platformID:=pid;
 specificID:=sid;
 languageID:=lid;
 fontDataSize:=0;
 fontData:=nil;
 cmap:=nil;
 cmapFormat:=0;
 numTables:=0;
 indexToLocFormat:=0;
 glyphs:=nil;
 glyphOffsetArray:=nil;
 strcopyright:='';
 strfamily:='';
 strsubfamily:='';
 strfullName:='';
 struniqueid:='';
 strversion:='';
 strpostscript:='';
 strtrademark:='';
 xMax:=0;
 xMin:=0;
 yMax:=0;
 yMin:=0;
 unitsPerEm:=0;
 ascender:=0;
 descender:=0;
 lineGap:=0;
 KerningTables:=nil;
 LastError:=TT_ERR_NoError;
 if ReadFontData(DataPointer,DataSize,CollectionIndex)=TT_ERR_NoError then begin
  if ProcessFontHeaderTable=TT_ERR_NoError then begin
   if ProcessMaximumProfileTable=TT_ERR_NoError then begin
    if ProcessNamingTable=TT_ERR_NoError then begin
     if ProcessIndexToLocationTable=TT_ERR_NoError then begin
      if ProcessCharacterMappingTable=TT_ERR_NoError then begin
       if ProcessGlyphDataTable=TT_ERR_NoError then begin
        if ProcessHorizontalHeaderTable=TT_ERR_NoError then begin
         if ProcessHorizontalMetricsTable=TT_ERR_NoError then begin
          if ProcessVerticalHeaderTable=TT_ERR_NoError then begin
           if ProcessVerticalMetricsTable=TT_ERR_NoError then begin
            if ProcessKerningTable<>TT_ERR_NoError then begin
             LastError:=TT_ERR_NoError; // Kerning table is only optional
            end;
           end;
          end;
         end;
        end;
       end;
      end;
     end;
    end;
   end;
  end;
 end;
end;

destructor TBeRoVectorCanvasTrueTypeFont.Destroy;
var i,j:integer;
begin
 for i:=0 to length(Glyphs)-1 do begin
  for j:=0 to length(Glyphs[i].Contours)-1 do begin
   setlength(Glyphs[i].Contours[j].Points,0);
  end;
  setlength(Glyphs[i].Contours,0);
 end;
 setlength(Glyphs,0);
 setlength(FontData,0);
 setlength(CMap,0);
 setlength(GlyphOffsetArray,0);
 for i:=0 to length(KerningTables)-1 do begin
  setlength(KerningTables[i].KerningPairs,0);
 end;
 setlength(KerningTables,0);
 inherited Destroy;
end;

function TBeRoVectorCanvasTrueTypeFont.ReadFontData(DataPointer:pointer;DataSize,CollectionIndex:integer):integer;
var i,tablelength,tabledirsize,tabledatasize:integer;
    tfd:array[0..OFFSET_TABLE_SIZE-1] of byte;
    tabledir:array of byte;
    DataPosition:integer;
 function read(var Buffer;LengthCounter:integer):integer;
 var RealSize:integer;
 begin
  RealSize:=LengthCounter;
  if (DataPosition+RealSize)>DataSize then begin
   RealSize:=DataSize-DataPosition;
  end;
  if RealSize<>LengthCounter then begin
   fillchar(Buffer,LengthCounter,#0);
  end;
  if RealSize>0 then begin
   move(PByteArray(DataPointer)^[DataPosition],Buffer,RealSize);
  end;
  inc(DataPosition,RealSize);
  result:=RealSize;
 end;
begin
 if not assigned(DataPointer) then begin
  LastError:=TT_ERR_UnableToOpenFile;
  result:=LastError;
  exit;
 end;

 DataPosition:=0;

 if read(tfd,OFFSET_TABLE_SIZE)<>OFFSET_TABLE_SIZE then begin
  LastError:=TT_ERR_CorruptFile;
  result:=LastError;
  exit;
 end;

 if ((tfd[0]=ord('t')) and (tfd[1]=ord('t')) and (tfd[2]=ord('c')) and (tfd[3]=ord('f'))) and
    ((tfd[4]=$00) or (tfd[5] in [$01,$02]) or (tfd[6]=$00) or (tfd[7]=$00)) then begin
  i:=CollectionIndex;
  if longword(i+0)>=toLONGWORD(tfd[8],tfd[9],tfd[10],tfd[11]) then begin
   LastError:=TT_ERR_InvalidFile;
   result:=LastError;
   exit;
  end;
  DataPosition:=12+(i*14);
  if read(tfd,4)<>4 then begin
   LastError:=TT_ERR_CorruptFile;
   result:=LastError;
   exit;
  end;
  DataPosition:=toLONGWORD(tfd[0],tfd[1],tfd[2],tfd[3]);
  if read(tfd,OFFSET_TABLE_SIZE)<>OFFSET_TABLE_SIZE then begin
   LastError:=TT_ERR_CorruptFile;
   result:=LastError;
   exit;
  end;
 end;

 if not (((tfd[0]=$00) and (tfd[1]=$01) and (tfd[2]=$00) and (tfd[3]=$00)) or
         ((tfd[0]=ord('1')) and (tfd[1]=$00) and (tfd[2]=$00) and (tfd[3]=$00)) or
         ((tfd[0]=ord('t')) and (tfd[1]=ord('r')) and (tfd[2]=ord('u')) and (tfd[3]=ord('e'))) or
         ((tfd[0]=ord('t')) and (tfd[1]=ord('y')) and (tfd[2]=ord('p')) and (tfd[3]=ord('1')))) then begin
  LastError:=TT_ERR_CorruptFile;
  result:=LastError;
  exit;
 end;

 numTables:=toWORD(tfd[4],tfd[5]); // Get the number of Tables in this font
 tabledirsize:=sizeof(longword)*4*numTables; // Calculate size of Table Directory
 tabledir:=nil;
 setlength(tabledir,tabledirsize);              // Allocate storage for Table Directory
 if length(tabledir)<>tabledirsize then begin
  LastError:=TT_ERR_OutOfMemory;
  result:=LastError;
  exit;
 end;

 tabledatasize:=0;
 i:=0;
 while i<tabledirsize do begin
  if read(tabledir[i],16)<>16 then begin
   setlength(tabledir,0);
   LastError:=TT_ERR_CorruptFile;
   result:=LastError;
   exit;
  end;
  tablelength:=toLONGWORD(tabledir[i+12],tabledir[i+13],tabledir[i+14],tabledir[i+15]);
  if (tablelength and 3)<>0 then begin
   inc(tablelength,4-(tablelength and 3));
  end;
  inc(tabledatasize,tablelength);
  inc(i,16);
 end;

 fontDataSize:=OFFSET_TABLE_SIZE+tabledirsize+tabledatasize; // Calculate size of entire font file
 setlength(fontData,fontDataSize); // Allocate space for all that data
 if length(fontData)<>fontDataSize then begin
  setlength(tabledir,0);
  LastError:=TT_ERR_OutOfMemory;
  result:=LastError;
  exit;
 end;
 fillchar(fontData[0],fontDataSize,#0);

 move(tfd[0],fontData[0],OFFSET_TABLE_SIZE); // Store the Offset Table
 move(tabledir[0],fontData[OFFSET_TABLE_SIZE],tabledirsize); // Store the Offset Table
 if read(fontData[OFFSET_TABLE_SIZE+tabledirsize],(fontDataSize-(OFFSET_TABLE_SIZE+tabledirsize)))<>((fontDataSize-(OFFSET_TABLE_SIZE+tabledirsize))) then begin // Store the rest of the font
{ setlength(tabledir,0);
  setlength(fontData,0);
  fontDataSize:=0;
  LastError:=TT_ERR_CorruptFile;
  result:=LastError;
  exit;}
 end;

 setlength(tabledir,0);

 LastError:=TT_ERR_NoError;
 result:=LastError;
end;

function TBeRoVectorCanvasTrueTypeFont.GetTableDirEntry(Tag:longword;var CheckSum,Offset,Size:longword):integer;
var i:integer;
    pos,currenttag:longword;
    found:boolean;
begin
 pos:=OFFSET_TABLE_SIZE;
 found:=false;
 for i:=0 to numTables-1 do begin
  currenttag:=toLONGWORD(fontData[pos],fontData[pos+1],fontData[pos+2],fontData[pos+3]);
  if currenttag=Tag then begin
   CheckSum:=toLONGWORD(fontData[pos+4],fontData[pos+5],fontData[pos+6],fontData[pos+7]);
   Offset:=toLONGWORD(fontData[pos+8],fontData[pos+9],fontData[pos+10],fontData[pos+11]);
   Size:=toLONGWORD(fontData[pos+12],fontData[pos+13],fontData[pos+14],fontData[pos+15]);
   found:=true;
   break;
  end;
  inc(pos,16);
 end;
 if found then begin
  LastError:=TT_ERR_NoError;
 end else begin
  LastError:=TT_ERR_TableNotFound;
 end;
 result:=LastError;
end;

function TBeRoVectorCanvasTrueTypeFont.ProcessFontHeaderTable:integer;
var pos,tag,checksum,offset,size,MagicNumber:longword;
begin
 tag:=toLONGWORD(byte('h'),byte('e'),byte('a'),byte('d'));
 result:=GetTableDirEntry(Tag,checksum,offset,size);
 if result<>TT_Err_NoError then exit;
 pos:=offset;
 inc(pos,sizeof(longword)); // Table version number
 inc(pos,sizeof(longword)); // Font revision number
 inc(pos,sizeof(longword)); // Checksum adjustment
 MagicNumber:=toLONGWORD(fontData[pos],fontData[pos+1],fontData[pos+2],fontData[pos+3]);
 inc(pos,sizeof(longword)); // Magic number
 if MagicNumber<>$5f0f3cf5 then begin
  LastError:=TT_ERR_InvalidFile;
  result:=LastError;
  exit;
 end;
 inc(pos,sizeof(word)); // Flags
 unitsPerEm:=toWORD(fontData[pos],fontData[pos+1]);
 inc(pos,sizeof(word));
 inc(pos,8); // Date created
 inc(pos,8); // Date modified
 xMin:=toSMALLINT(fontData[pos],fontData[pos+1]);
 inc(pos,sizeof(smallint));
 yMin:=toSMALLINT(fontData[pos],fontData[pos+1]);
 inc(pos,sizeof(smallint));
 xMax:=toSMALLINT(fontData[pos],fontData[pos+1]);
 inc(pos,sizeof(smallint));
 yMax:=toSMALLINT(fontData[pos],fontData[pos+1]);
 inc(pos,sizeof(smallint));
 inc(pos,sizeof(word)); // Mac-style
 inc(pos,sizeof(word)); // Lowest rec pen
 inc(pos,sizeof(smallint)); // Font direction hint
 indexToLocFormat:=toSMALLINT(fontData[pos],fontData[pos+1]);
//inc(pos,sizeof(smallint));
 LastError:=TT_ERR_NoError;
 result:=LastError;
end;

function TBeRoVectorCanvasTrueTypeFont.ProcessMaximumProfileTable:integer;
var pos,tag,checksum,offset,size:longword;
    n:word;
    i:integer;
begin
 tag:=toLONGWORD(byte('m'),byte('a'),byte('x'),byte('p'));
 result:=GetTableDirEntry(Tag,checksum,offset,size);
 if result<>TT_Err_NoError then exit;
 pos:=offset;
 inc(pos,sizeof(longword)); // Table Version number
 n:=toWORD(fontData[pos],fontData[pos+1]);
//inc(pos,sizeof(word));
 setlength(Glyphs,n);
 if length(Glyphs)<>n then begin
  LastError:=TT_ERR_OutOfMemory;
  result:=LastError;
  exit;
 end;
 for i:=0 to length(Glyphs)-1 do begin
  fillchar(Glyphs[i],sizeof(TBeRoVectorCanvasTrueTypeGlyph),#0);
 end;
 LastError:=TT_ERR_NoError;
 result:=LastError;
end;

function TBeRoVectorCanvasTrueTypeFont.ProcessNamingTable:integer;
var pos,tag,checksum,offset,size,numNameRecords,stringStorageOffset,i:longword;
    thisPlatformID,thisSpecificID,thisLanguageID,thisNameID,thisStringLength,thisStringOffset:word;
    namefound:boolean;
    ws:^widestring;
    s:ansistring;
begin
 tag:=toLONGWORD(byte('n'),byte('a'),byte('m'),byte('e'));
 result:=GetTableDirEntry(Tag,checksum,offset,size);
 if result<>TT_Err_NoError then exit;
 pos:=offset;
 inc(pos,sizeof(word)); // Format Selector
 numNameRecords:=toWORD(fontData[pos],fontData[pos+1]);
 inc(pos,sizeof(word));
 stringStorageOffset:=toWORD(fontData[pos],fontData[pos+1])+offset;
 inc(pos,sizeof(word));
 for i:=1 to numNameRecords do begin
  thisPlatformID:=toWORD(fontData[pos],fontData[pos+1]);
  inc(pos,sizeof(word));
  thisSpecificID:=toWORD(fontData[pos],fontData[pos+1]);
  inc(pos,sizeof(word));
  thisLanguageID:=toWORD(fontData[pos],fontData[pos+1]);
  inc(pos,sizeof(word));
  thisNameID:=toWORD(fontData[pos],fontData[pos+1]);
  inc(pos,sizeof(word));
  thisStringLength:=toWORD(fontData[pos],fontData[pos+1]);
  inc(pos,sizeof(word));
  thisStringOffset:=toWORD(fontData[pos],fontData[pos+1]);
  inc(pos,sizeof(word));
  if (thisPlatformID<>PlatformID) or (thisSpecificID<>SpecificID) or (thisLanguageID<>LanguageID) then begin
   continue;
  end;
  namefound:=false;
  ws:=nil;
  case thisNameID of
   NID_Copyright:begin
    namefound:=true;
    ws:=@strcopyright;
   end;
   NID_Family:begin
    namefound:=true;
    ws:=@strfamily;
   end;
   NID_Subfamily:begin
    namefound:=true;
    ws:=@strsubfamily;
   end;
   NID_UniqueID:begin
    namefound:=true;
    ws:=@struniqueid;
   end;
   NID_FullName:begin
    namefound:=true;
    ws:=@strfullname;
   end;
   NID_Version:begin
    namefound:=true;
    ws:=@strversion;
   end;
   NID_PostscriptName:begin
    namefound:=true;
    ws:=@strpostscript;
   end;
   NID_Trademark:begin
    namefound:=true;
    ws:=@strtrademark;
   end;
  end;
  if namefound then begin
   case ThisPlatformID of
    PID_Microsoft:begin
     setlength(ws^,thisStringLength div 2);
     if length(ws^)<>(thisStringLength div 2) then begin
      LastError:=TT_ERR_OutOfMemory;
      result:=LastError;
      exit;
     end;
     move(fontData[stringStorageOffset+thisStringOffset],ws^[1],thisStringLength div 2);
    end;
    else begin
     setlength(ws^,thisStringLength);
     if length(ws^)<>thisStringLength then begin
      LastError:=TT_ERR_OutOfMemory;
      result:=LastError;
      exit;
     end;
     setlength(s,thisStringLength);
     if length(s)<>thisStringLength then begin
      LastError:=TT_ERR_OutOfMemory;
      result:=LastError;
      exit;
     end;
     move(fontData[stringStorageOffset+thisStringOffset],s[1],thisStringLength);
     ws^:=s;
    end;
   end;
  end;
 end;
 LastError:=TT_ERR_NoError;
 result:=LastError;
end;

function TBeRoVectorCanvasTrueTypeFont.ProcessIndexToLocationTable:integer;
var pos,tag,checksum,offset,size,thisOffset:longword;
    i:integer;
begin
 tag:=toLONGWORD(byte('l'),byte('o'),byte('c'),byte('a'));
 result:=GetTableDirEntry(Tag,checksum,offset,size);
 if result<>TT_Err_NoError then exit;
 pos:=offset;
 setlength(glyphOffsetArray,length(glyphs)+1);
 if length(glyphOffsetArray)<>(length(glyphs)+1) then begin
  LastError:=TT_ERR_OutOfMemory;
  result:=LastError;
  exit;
 end;
 thisOffset:=0;
 for i:=0 to length(glyphs) do begin
  case indexToLocFormat of
   0:begin
    thisOffset:=toWORD(fontData[pos],fontData[pos+1])*2;
    inc(pos,sizeof(word));
   end;
   1:begin
    thisOffset:=toLONGWORD(fontData[pos],fontData[pos+1],fontData[pos+2],fontData[pos+3]);
    inc(pos,sizeof(longword));
   end;
   else begin
    LastError:=TT_ERR_CorruptFile;
    result:=LastError;
    exit;
   end;
  end;
  glyphOffsetArray[i]:=thisOffset;
 end;
 LastError:=TT_ERR_NoError;
 result:=LastError;
end;

function TBeRoVectorCanvasTrueTypeFont.ProcessGlyphDataTable:integer;
const ARGS_ARE_WORDS=$0001;
      ARGS_ARE_XY_VALUES=$0002;
      ROUND_XY_TO_GRID=$0004;
      WE_HAVE_A_SCALE=$0008;
      MORE_COMPONENTS=$0020;
      WE_HAVE_AN_XY_SCALE=$0040;
      WE_HAVE_A_2X2=$0080;
      WE_HAVE_INSTR=$0100;
      USE_MY_METRICS=$0200;
      OVERLAP_COMPOUND=$0400;
      SCALED_COMPONENT_OFFSET=$0800;
      UNSCALED_COMPONENT_OFFSET=$1000;
type pgflags=^tgflags;
     tgflags=array[word] of boolean;
var cflags,cindex,ccount,pos,tag,checksum,offset,size,thisOffset,currGlyphOffset,nextGlyphOffset,currGlyphLength,instructionLength:longword;
    i,j,carg1,carg2,cxx,cyx,cxy,cyy,currentFlag,gi,numContours,numberOfPoints,repeatcount,startPoint,endPoint,xbyte,ybyte,xword,yword,tx,ty:integer;
    endPtsOfContours:array of word;
    flags:array of byte;
    gflags:pgflags;
 procedure ProcessGlyph(gi:integer);
  function MulFix(a,b:integer):integer;
  var s:integer;
  begin
   s:=1;
   if a<0 then begin
    a:=-a;
    s:=-1;
   end;
   if b<0 then begin
    b:=-b;
    s:=-s;
   end;
   result:=((int64(a)*int64(b))+$8000) div 65536;
   if s<0 then begin
    result:=-result;
   end;
  end;
  function SqrtFixed(x:integer):integer;
  var rh,rl,td:longword;
      c:integer;
  begin
   result:=0;
   if x>0 then begin
    rh:=0;
    rl:=x;
    c:=24;
    repeat
     rh:=(rh shl 2) or (rl shr 30);
     rl:=rl shl 2;
     result:=result shl 1;
     td:=(result shl 1)+1;
     if rh>=td then begin
      dec(rh,td);
      inc(result);
     end;
     dec(c);
    until c=0;
   end;
  end;
  procedure Transform(var x,y:integer;xx,yx,xy,yy:integer);
  var xz,yz:integer;
  begin
   xz:=MulFix(x,xx)+MulFix(y,xy);
   yz:=MulFix(x,yx)+MulFix(y,yy);
   x:=xz;
   y:=yz;
  end;
  procedure GetPointXY(gi,pi:integer;var x,y:integer);
  var i,j:integer;
  begin
   x:=0;
   y:=0;
   j:=0;
   for i:=0 to length(Glyphs[gi].Contours)-1 do begin
    if (pi-j)<length(Glyphs[gi].Contours[i].Points) then begin
     x:=Glyphs[gi].Contours[i].Points[pi-j].x;
     y:=Glyphs[gi].Contours[i].Points[pi-j].y;
     exit;
    end;
    inc(j,length(Glyphs[gi].Contours[i].Points));
   end;
  end;
  procedure ProcessSubGlyph(tgi:integer;const sg:TBeRoVectorCanvasTrueTypeCompositeSubGlyph;x,y,xx,yx,xy,yy,p1c,p2c:integer);
  const fi65536=1.0/65536;
  var HaveScale:boolean;
      tgip1,sgpc,ps,pe,tgic1,sgcc,i,j,k,l,p1,p2,cs,ce,cx,cy,p1x,p1y,p2x,p2y,
      numpoints:integer;
      mxs,mys:double;
  begin
   tgip1:=length(Glyphs[tgi].Points);
   sgpc:=length(Glyphs[sg.Glyph].Points);
   setlength(Glyphs[tgi].Points,tgip1+sgpc);
   numpoints:=length(Glyphs[tgi].Points);
   ps:=tgip1;
   pe:=tgip1+sgpc-1;
   for i:=ps to pe do begin
    Glyphs[tgi].Points[i]:=Glyphs[sg.Glyph].Points[i-tgip1];
   end;
   tgic1:=length(Glyphs[tgi].Contours);
   sgcc:=length(Glyphs[sg.Glyph].Contours);
   setlength(Glyphs[tgi].Contours,tgic1+sgcc);
   cs:=tgic1;
   ce:=tgic1+sgcc-1;
   for i:=cs to ce do begin
    Glyphs[tgi].Contours[i].Points:=copy(Glyphs[sg.Glyph].Contours[i-tgic1].Points,0,length(Glyphs[sg.Glyph].Contours[i-tgic1].Points));
   end;
   HaveScale:=(sg.Flags and (WE_HAVE_A_SCALE or WE_HAVE_AN_XY_SCALE or WE_HAVE_A_2X2))<>0;
   if HaveScale then begin
    for i:=ps to pe do begin
     Transform(Glyphs[tgi].Points[i].x,Glyphs[tgi].Points[i].y,xx,yx,xy,yy);
    end;
    for i:=cs to ce do begin
     for j:=0 to length(Glyphs[tgi].Contours[i].Points)-1 do begin
      Transform(Glyphs[tgi].Contours[i].Points[j].X,Glyphs[tgi].Contours[i].Points[j].Y,xx,yx,xy,yy);
     end;
    end;
   end;
   if (sg.Flags and ARGS_ARE_XY_VALUES)<>0 then begin
    cx:=sg.x;
    cy:=sg.y;
   end else begin
    p1:=sg.x;
    p2:=sg.y+ps;
    if (p1>=0) and (p2>=0) and (p1<ps) and (p2<numpoints) then begin
     p1x:=Glyphs[tgi].Points[p1].x;
     p1y:=Glyphs[tgi].Points[p1].y;
     p2x:=Glyphs[tgi].Points[p2].x;
     p2y:=Glyphs[tgi].Points[p2].y;
     cx:=p1x-p2x;
     cy:=p1y-p2y;
    end else begin
     cx:=0;
     cy:=0;
    end;
   end;
   if (cx<>0) or (cy<>0) then begin
    if HaveScale and ((sg.Flags and UNSCALED_COMPONENT_OFFSET)=0) then begin
{$ifdef cpuarm}
     cx:=MulFix(cx,SqrtFixed(MulFix(sg.xx,sg.xx)+MulFix(sg.xy,sg.xy)));
     cy:=MulFix(cy,SqrtFixed(MulFix(sg.yx,sg.yx)+MulFix(sg.yy,sg.yy)));
{$else}
     cx:=round(cx*sqrt(sqr(sg.xx*fi65536)+sqr(sg.xy*fi65536)));
     cy:=round(cy*sqrt(sqr(sg.yx*fi65536)+sqr(sg.yy*fi65536)));
{$endif}
    end;
   end;
   if (sg.Flags and ROUND_XY_TO_GRID)<>0 then begin
    cx:=(cx+33) and not 63;
    cy:=(cy+33) and not 63;
   end;
   if (cx<>0) or (cy<>0) then begin
    for i:=ps to pe do begin
     inc(Glyphs[tgi].Points[i].x,cx);
     inc(Glyphs[tgi].Points[i].y,cy);
    end;
    for i:=cs to ce do begin
     for j:=0 to length(Glyphs[tgi].Contours[i].Points)-1 do begin
      inc(Glyphs[tgi].Contours[i].Points[j].X,cx);
      inc(Glyphs[tgi].Contours[i].Points[j].Y,cy);
     end;
    end;
   end;
  end;
  procedure ProcessGlyphEx(tgi,cgi,x,y,xx,yx,xy,yy:integer);
  var i,p1,p2:integer;
  begin
   if gflags^[cgi] then begin
    exit;
   end;
   gflags^[cgi]:=true;
   p1:=0;
   for i:=0 to length(Glyphs[gi].CompositeSubGlyphs)-1 do begin
    p2:=p1;
    p1:=length(Glyphs[gi].Points);
    ProcessSubGlyph(tgi,Glyphs[gi].CompositeSubGlyphs[i],x,y,xx,yx,xy,yy,p1,p2);
   end;
   gflags^[cgi]:=false;
  end;
 begin
  ProcessGlyphEx(gi,gi,0,0,$10000,0,0,$10000);
  setlength(Glyphs[gi].CompositeSubGlyphs,0);
 end;
begin
 endPtsOfContours:=nil;
 flags:=nil;
 tag:=toLONGWORD(byte('g'),byte('l'),byte('y'),byte('f'));
 result:=GetTableDirEntry(Tag,checksum,offset,size);
 if result<>TT_Err_NoError then exit;
 for gi:=0 to length(Glyphs)-1 do begin
  currGlyphOffset:=glyphOffsetArray[gi];
  nextGlyphOffset:=glyphOffsetArray[gi+1];
  currGlyphLength:=nextGlyphOffset-currGlyphOffset;
  glyphs[gi].UseMetricsFrom:=-1;
  if currGlyphLength=0 then begin
   glyphs[gi].Contours:=nil;
   glyphs[gi].CompositeSubGlyphs:=nil;
   glyphs[gi].xMin:=0;
   glyphs[gi].yMin:=0;
   glyphs[gi].xMax:=0;
   glyphs[gi].yMax:=0;
   continue;
  end;
  pos:=offset+currGlyphOffset;
  numContours:=toSMALLINT(fontData[pos],fontData[pos+1]);
  inc(pos,sizeof(smallint));
  glyphs[gi].xMin:=toSMALLINT(fontData[pos],fontData[pos+1]);
  inc(pos,sizeof(smallint));
  glyphs[gi].yMin:=toSMALLINT(fontData[pos],fontData[pos+1]);
  inc(pos,sizeof(smallint));
  glyphs[gi].xMax:=toSMALLINT(fontData[pos],fontData[pos+1]);
  inc(pos,sizeof(smallint));
  glyphs[gi].yMax:=toSMALLINT(fontData[pos],fontData[pos+1]);
  inc(pos,sizeof(smallint));
  if numContours<=0 then begin
   glyphs[gi].Contours:=nil;
   glyphs[gi].CompositeSubGlyphs:=nil;
   if numContours=-1 then begin
    repeat
     i:=length(glyphs[gi].CompositeSubGlyphs);
     setlength(glyphs[gi].CompositeSubGlyphs,i+1);
     cflags:=toWORD(fontData[pos],fontData[pos+1]);
     inc(pos,sizeof(word));
     cindex:=toWORD(fontData[pos],fontData[pos+1]);
     inc(pos,sizeof(word));
     if (cflags and USE_MY_METRICS)<>0 then begin
      Glyphs[gi].UseMetricsFrom:=cindex;
     end;
     ccount:=2;
     if (cflags and ARGS_ARE_WORDS)<>0 then begin
      inc(ccount,2);
     end;
     if (cflags and WE_HAVE_A_SCALE)<>0 then begin
      inc(ccount,2);
     end else if (cflags and WE_HAVE_AN_XY_SCALE)<>0 then begin
      inc(ccount,4);
     end else if (cflags and WE_HAVE_A_2X2)<>0 then begin
      inc(ccount,8);
     end;
     if (cflags and ARGS_ARE_WORDS)<>0 then begin
      carg1:=toSMALLINT(fontData[pos],fontData[pos+1]);
      inc(pos,sizeof(smallint));
      carg2:=toSMALLINT(fontData[pos],fontData[pos+1]);
      inc(pos,sizeof(smallint));
     end else begin
      carg1:=SHORTINT(fontData[pos]);
      inc(pos);
      carg2:=SHORTINT(fontData[pos]);
      inc(pos);
     end;
     cxx:=$10000;
     cyy:=$10000;
     cxy:=0;
     cyx:=0;
     if (cflags and WE_HAVE_A_SCALE)<>0 then begin
      cxx:=toSMALLINT(fontData[pos],fontData[pos+1])*4;
      inc(pos,sizeof(smallint));
      cyy:=cxx;
     end else if (cflags and WE_HAVE_AN_XY_SCALE)<>0 then begin
      cxx:=toSMALLINT(fontData[pos],fontData[pos+1])*4;
      inc(pos,sizeof(smallint));
      cyy:=toSMALLINT(fontData[pos],fontData[pos+1])*4;
      inc(pos,sizeof(smallint));
     end else if (cflags and WE_HAVE_A_2X2)<>0 then begin
      cxx:=toSMALLINT(fontData[pos],fontData[pos+1])*4;
      inc(pos,sizeof(smallint));
      cyx:=toSMALLINT(fontData[pos],fontData[pos+1])*4;
      inc(pos,sizeof(smallint));
      cxy:=toSMALLINT(fontData[pos],fontData[pos+1])*4;
      inc(pos,sizeof(smallint));
      cyy:=toSMALLINT(fontData[pos],fontData[pos+1])*4;
      inc(pos,sizeof(smallint));
     end;
     glyphs[gi].CompositeSubGlyphs[i].Flags:=cflags;
     glyphs[gi].CompositeSubGlyphs[i].Glyph:=cindex;
     glyphs[gi].CompositeSubGlyphs[i].x:=carg1;
     glyphs[gi].CompositeSubGlyphs[i].y:=carg2;
     glyphs[gi].CompositeSubGlyphs[i].xx:=cxx;
     glyphs[gi].CompositeSubGlyphs[i].yx:=cyx;
     glyphs[gi].CompositeSubGlyphs[i].xy:=cxy;
     glyphs[gi].CompositeSubGlyphs[i].yy:=cyy;
    until (cflags and MORE_COMPONENTS)=0;
   end;
   continue;
  end;
  glyphs[gi].CompositeSubGlyphs:=nil;
  setlength(glyphs[gi].Contours,numContours);
  if length(glyphs[gi].Contours)<>numContours then begin
   LastError:=TT_ERR_OutOfMemory;
   result:=LastError;
   exit;
  end;
  setlength(endPtsOfContours,numContours);
  if length(endPtsOfContours)<>numContours then begin
   LastError:=TT_ERR_OutOfMemory;
   result:=LastError;
   exit;
  end;
  for i:=0 to length(endPtsOfContours)-1 do begin
   endPtsOfContours[i]:=toWORD(fontData[pos],fontData[pos+1]);
   inc(pos,sizeof(word));
  end;
  numberOfPoints:=endPtsOfContours[length(endPtsOfContours)-1]+1;
  instructionLength:=toWORD(fontData[pos],fontData[pos+1]);
  inc(pos,sizeof(word));
  inc(pos,instructionLength);
  setlength(flags,numberOfPoints);
  if length(flags)<>numberOfPoints then begin
   LastError:=TT_ERR_OutOfMemory;
   result:=LastError;
   exit;
  end;
  i:=0;
  while i<numberOfPoints do begin
   flags[i]:=fontData[pos];
   inc(pos);
   if (flags[i] and 8)<>0 then begin
    repeatcount:=fontData[pos];
    inc(pos);
    while repeatcount>0 do begin
     inc(i);
     flags[i]:=flags[i-1];
     dec(repeatcount);
    end;
   end;
   inc(i);
  end;
  setlength(glyphs[gi].Points,numberOfPoints);
  for i:=0 to numberOfPoints-1 do begin
   currentFlag:=flags[i];
   glyphs[gi].Points[i].OnCurve:=(currentFlag and 1)<>0;
   if (currentFlag and 2)<>0 then begin
    xByte:=fontData[pos];
    inc(pos);
    if (currentFlag and 16)<>0 then begin
     glyphs[gi].Points[i].x:=xbyte;
    end else begin
     glyphs[gi].Points[i].x:=-xbyte;
    end;
   end else begin
    if (currentFlag and 16)<>0 then begin
     glyphs[gi].Points[i].x:=0;
    end else begin
     xword:=toSMALLINT(fontData[pos],fontData[pos+1]);
     inc(pos,sizeof(smallint));
     glyphs[gi].Points[i].x:=xword;
    end;
   end;
  end;
  for i:=0 to numberOfPoints-1 do begin
   currentFlag:=flags[i];
   if (currentFlag and 4)<>0 then begin
    yByte:=fontData[pos];
    inc(pos);
    if (currentFlag and 32)<>0 then begin
     glyphs[gi].Points[i].y:=ybyte;
    end else begin
     glyphs[gi].Points[i].y:=-ybyte;
    end;
   end else begin
    if (currentFlag and 32)<>0 then begin
     glyphs[gi].Points[i].y:=0;
    end else begin
     yword:=toSMALLINT(fontData[pos],fontData[pos+1]);
     inc(pos,sizeof(smallint));
     glyphs[gi].Points[i].y:=yword;
    end;
   end;
  end;
  startPoint:=0;
  for i:=0 to numContours-1 do begin
   endPoint:=endPtsOfContours[i];
   setlength(glyphs[gi].contours[i].Points,endPoint-startPoint+1);
   if length(glyphs[gi].contours[i].Points)<>(endPoint-startPoint+1) then begin
    LastError:=TT_ERR_OutOfMemory;
    result:=LastError;
    exit;
   end;
   for j:=startpoint to endpoint do begin
    glyphs[gi].contours[i].Points[j-startPoint]:=glyphs[gi].Points[j];
   end;
   startPoint:=endPoint+1;
  end;
 end;
 setlength(endPtsOfContours,0);
 setlength(flags,0);
 for gi:=0 to length(Glyphs)-1 do begin
  tx:=0;
  ty:=0;
  for i:=0 to length(glyphs[gi].Contours)-1 do begin
   for j:=0 to length(glyphs[gi].Contours[i].Points)-1 do begin
    inc(tx,glyphs[gi].Contours[i].Points[j].x);
    inc(ty,glyphs[gi].Contours[i].Points[j].y);
    glyphs[gi].Contours[i].Points[j].x:=tx;
    glyphs[gi].Contours[i].Points[j].y:=ty;
   end;
  end;
 end;
 new(gflags);
 fillchar(gflags^,sizeof(tgflags),#0);
 for gi:=0 to length(Glyphs)-1 do begin
  ProcessGlyph(gi);
 end;
 dispose(gflags);
 LastError:=TT_ERR_NoError;
 result:=LastError;
end;

function TBeRoVectorCanvasTrueTypeFont.ProcessHorizontalHeaderTable:integer;
var pos,tag,checksum,offset,size:longword;
begin
 tag:=toLONGWORD(byte('h'),byte('h'),byte('e'),byte('a'));
 result:=GetTableDirEntry(Tag,checksum,offset,size);
 if result<>TT_Err_NoError then begin
  tag:=toLONGWORD(byte('v'),byte('h'),byte('e'),byte('a'));
  result:=GetTableDirEntry(Tag,checksum,offset,size);
  exit;
 end;
 pos:=offset;
 inc(pos,sizeof(longword)); // Table Version number
 ascender:=toSMALLINT(fontData[pos],fontData[pos+1]);
 inc(pos,sizeof(smallint));
 descender:=toSMALLINT(fontData[pos],fontData[pos+1]);
 inc(pos,sizeof(smallint));
 lineGap:=toSMALLINT(fontData[pos],fontData[pos+1]);
 inc(pos,sizeof(smallint));
 AdvanceWidthMax:=toWORD(fontData[pos],fontData[pos+1]);
 inc(pos,sizeof(word));
 inc(pos,sizeof(smallint)); // MinLeftSideBearing
 inc(pos,sizeof(smallint)); // MinRightSideBearing
 inc(pos,sizeof(smallint)); // XMaxExtent
 inc(pos,sizeof(smallint)); // CaretSlopeRise
 inc(pos,sizeof(smallint)); // CaretSlopeRun
 inc(pos,sizeof(word)*5); // 5 reserved words
 inc(pos,sizeof(smallint)); // MetricDataFormat
 numberOfHMetrics:=toWORD(fontData[pos],fontData[pos+1]);
 LastError:=TT_ERR_NoError;
 result:=LastError;
end;

function TBeRoVectorCanvasTrueTypeFont.ProcessHorizontalMetricsTable:integer;
var pos,tag,checksum,offset,size:longword;
    i,j:integer;
begin
 tag:=toLONGWORD(byte('h'),byte('m'),byte('t'),byte('x'));
 result:=GetTableDirEntry(Tag,checksum,offset,size);
 if result<>TT_Err_NoError then begin
  tag:=toLONGWORD(byte('v'),byte('m'),byte('t'),byte('x'));
  result:=GetTableDirEntry(Tag,checksum,offset,size);
  exit;
 end;
 pos:=offset;
 i:=0;
 while i<NumberOfHMetrics do begin
  glyphs[i].AdvanceWidth:=toWORD(fontData[pos],fontData[pos+1]);
  inc(pos,sizeof(word));
  glyphs[i].LeftSideBearing:=toSMALLINT(fontData[pos],fontData[pos+1]);
  inc(pos,sizeof(smallint));
  glyphs[i].RightSideBearing:=glyphs[i].AdvanceWidth-(glyphs[i].LeftSideBearing+(glyphs[i].xmax-glyphs[i].xmin));
  inc(i);
 end;
 if numberOfHMetrics=length(glyphs) then begin
  result:=LastError;
  exit;
 end;
 j:=glyphs[i-1].AdvanceWidth;
 while i<length(Glyphs) do begin
  glyphs[i].AdvanceWidth:=j;
  glyphs[i].LeftSideBearing:=toSMALLINT(fontData[pos],fontData[pos+1]);
  inc(pos,sizeof(smallint));
  glyphs[i].RightSideBearing:=glyphs[i].AdvanceWidth-(glyphs[i].LeftSideBearing+(glyphs[i].xmax-glyphs[i].xmin));
  inc(i);
 end;
 LastError:=TT_ERR_NoError;
 result:=LastError;
end;

function TBeRoVectorCanvasTrueTypeFont.ProcessVerticalHeaderTable:integer;
var pos,tag,checksum,offset,size:longword;
begin
 tag:=toLONGWORD(byte('v'),byte('h'),byte('e'),byte('a'));
 result:=GetTableDirEntry(Tag,checksum,offset,size);
 if result<>TT_Err_NoError then begin
  tag:=toLONGWORD(byte('h'),byte('h'),byte('e'),byte('a'));
  result:=GetTableDirEntry(Tag,checksum,offset,size);
  exit;
 end;
 pos:=offset;
 inc(pos,sizeof(longword)); // Table Version number
 ascender:=toSMALLINT(fontData[pos],fontData[pos+1]);
 inc(pos,sizeof(smallint));
 descender:=toSMALLINT(fontData[pos],fontData[pos+1]);
 inc(pos,sizeof(smallint));
 lineGap:=toSMALLINT(fontData[pos],fontData[pos+1]);
 inc(pos,sizeof(smallint));
 AdvanceHeightMax:=toWORD(fontData[pos],fontData[pos+1]);
 inc(pos,sizeof(word));
 inc(pos,sizeof(smallint)); // MinTopSideBearing
 inc(pos,sizeof(smallint)); // MinBottomSideBearing
 inc(pos,sizeof(smallint)); // YMaxExtent
 inc(pos,sizeof(smallint)); // CaretSlopeRise
 inc(pos,sizeof(smallint)); // CaretSlopeRun
 inc(pos,sizeof(word)*5); // 5 reserved words
 inc(pos,sizeof(smallint)); // MetricDataFormat
 numberOfVMetrics:=toWORD(fontData[pos],fontData[pos+1]);
 LastError:=TT_ERR_NoError;
 result:=LastError;
end;

function TBeRoVectorCanvasTrueTypeFont.ProcessVerticalMetricsTable:integer;
var pos,tag,checksum,offset,size:longword;
    i,j:integer;
begin
 tag:=toLONGWORD(byte('v'),byte('m'),byte('t'),byte('x'));
 result:=GetTableDirEntry(Tag,checksum,offset,size);
 if result<>TT_Err_NoError then begin
  tag:=toLONGWORD(byte('h'),byte('m'),byte('t'),byte('x'));
  result:=GetTableDirEntry(Tag,checksum,offset,size);
  exit;
 end;
 pos:=offset;
 i:=0;
 while i<NumberOfVMetrics do begin
  glyphs[i].AdvanceHeight:=toWORD(fontData[pos],fontData[pos+1]);
  inc(pos,sizeof(word));
  glyphs[i].TopSideBearing:=toSMALLINT(fontData[pos],fontData[pos+1]);
  inc(pos,sizeof(smallint));
  glyphs[i].BottomSideBearing:=glyphs[i].AdvanceHeight-(glyphs[i].TopSideBearing+glyphs[i].ymax-glyphs[i].ymin);
  inc(i);
 end;
 if numberOfVMetrics=length(glyphs) then begin
  result:=LastError;
  exit;
 end;
 j:=glyphs[i-1].AdvanceHeight;
 while i<length(Glyphs) do begin
  glyphs[i].AdvanceWidth:=j;
  glyphs[i].TopSideBearing:=toSMALLINT(fontData[pos],fontData[pos+1]);
  inc(pos,sizeof(smallint));
  glyphs[i].BottomSideBearing:=glyphs[i].AdvanceHeight-(glyphs[i].TopSideBearing+glyphs[i].ymax-glyphs[i].ymin);
  inc(i);
 end;
 LastError:=TT_ERR_NoError;
 result:=LastError;
end;

function TBeRoVectorCanvasTrueTypeFont.ProcessKerningTable:integer;
var pos,tag,checksum,offset,size,v1,v2,SubTableSize,Next:longword;
    NumberOfSubTables,i,j,NumKerningPairs:integer;
    CoverageFormat,CoverageFlags:byte;
    DoNeedSort:boolean;
 procedure QuickSort(var KerningPairs:TBeRoVectorCanvasTrueTypeKerningPairs;l,r:integer);
 var i,j:integer;
     p,t:TBeRoVectorCanvasTrueTypeKerningPair;
     pv:longword;
 begin
  repeat
   i:=l;
   j:=r;
   p:=KerningPairs[(l+r) div 2];
   pv:=((p.Left shl 16) or p.Right);
   repeat
    while ((KerningPairs[i].Left shl 16) or KerningPairs[i].Right)<pv do inc(i);
    while ((KerningPairs[j].Left shl 16) or KerningPairs[j].Right)>pv do dec(j);
    if i<=j then begin
     t:=KerningPairs[i];
     KerningPairs[i]:=KerningPairs[j];
     KerningPairs[j]:=t;
     inc(i);
     dec(j);
    end;
   until i>j;
   if l<j then begin
    QuickSort(KerningPairs,l,j);
   end;
   l:=i;
  until i>=r;
 end;
 procedure BeRoSort(var KerningPairs:TBeRoVectorCanvasTrueTypeKerningPairs);
 var i:integer;
     t:TBeRoVectorCanvasTrueTypeKerningPair;
 begin
  i:=0;
  while i<length(KerningPairs)-1 do begin
   if ((KerningPairs[i].Left shl 16) or KerningPairs[i].Right)>((KerningPairs[i+1].Left shl 16) or KerningPairs[i+1].Right) then begin
    t:=KerningPairs[i];
    KerningPairs[i]:=KerningPairs[i+1];
    KerningPairs[i+1]:=t;
    if i>0 then begin
     dec(i);
    end else begin
     inc(i);
    end;
   end else begin
    inc(i);
   end;
  end;
 end;
begin
 tag:=toLONGWORD(byte('k'),byte('e'),byte('r'),byte('n'));
 result:=GetTableDirEntry(Tag,checksum,offset,size);
 if result<>TT_Err_NoError then exit;
 pos:=offset;
 inc(pos,sizeof(word)); // Table Version number
 NumberOfSubTables:=toWORD(fontData[pos],fontData[pos+1]);
 inc(pos,sizeof(word));

 setlength(KerningTables,NumberOfSubTables);
 if length(KerningTables)<>NumberOfSubTables then begin
  LastError:=TT_ERR_OutOfMemory;
  result:=LastError;
  exit;
 end;

 for i:=0 to NumberOfSubTables-1 do begin
  inc(pos,sizeof(word)); // Subtable version number
  SubTableSize:=toWORD(fontData[pos],fontData[pos+1]);
  inc(pos,sizeof(word)); // Subtable size
  CoverageFormat:=fontData[pos];
  CoverageFlags:=fontData[pos+1];
  inc(pos,2);
  Next:=Pos+SubTableSize-6;

  if ((CoverageFlags and (2 or 4))<>0) or (CoverageFormat<>0) then begin
   Pos:=Next;
   continue;
  end;

  KerningTables[i].Horizontal:=(CoverageFlags and 1)<>0;
  KerningTables[i].ValueOverride:=(CoverageFlags and 8)<>0;

  NumKerningPairs:=toWORD(fontData[pos],fontData[pos+1]);
  inc(pos,sizeof(word));

  inc(pos,sizeof(word)); // Search range
  inc(pos,sizeof(word)); // Entry selector
  inc(pos,sizeof(word)); // Range shift

  KerningTables[i].KerningPairs:=nil;
  setlength(KerningTables[i].KerningPairs,NumKerningPairs);
  if length(KerningTables[i].KerningPairs)<>NumKerningPairs then begin
   LastError:=TT_ERR_OutOfMemory;
   result:=LastError;
   exit;
  end;

  if (length(KerningTables[i].KerningPairs)*12)>length(fontData) then begin
   LastError:=TT_ERR_CorruptFile;
   result:=LastError;
   exit;
  end;

  for j:=0 to length(KerningTables[i].KerningPairs)-1 do begin
   KerningTables[i].KerningPairs[j].Left:=toWORD(fontData[pos],fontData[pos+1]);
   inc(pos,sizeof(word));
   KerningTables[i].KerningPairs[j].Right:=toWORD(fontData[pos],fontData[pos+1]);
   inc(pos,sizeof(word));
   KerningTables[i].KerningPairs[j].Value:=toSMALLINT(fontData[pos],fontData[pos+1]);
   inc(pos,sizeof(smallint));
  end;

  KerningTables[i].BinarySearch:=false;
  if length(KerningTables[i].KerningPairs)<>0 then begin
   DoNeedSort:=false;
   for j:=1 to length(KerningTables[i].KerningPairs)-1 do begin
    v1:=(KerningTables[i].KerningPairs[j-1].Left shl 16) or KerningTables[i].KerningPairs[j-1].Right;
    v2:=(KerningTables[i].KerningPairs[j].Left shl 16) or KerningTables[i].KerningPairs[j].Right;
    if v1>=v2 then begin
     DoNeedSort:=true;
     break;
    end;
   end;
   if DoNeedSort then begin
    QuickSort(KerningTables[i].KerningPairs,0,length(KerningTables[i].KerningPairs)-1);
    BeRoSort(KerningTables[i].KerningPairs);
   end;
   KerningTables[i].BinarySearch:=true;
   for j:=1 to length(KerningTables[i].KerningPairs)-1 do begin
    v1:=(KerningTables[i].KerningPairs[j-1].Left shl 16) or KerningTables[i].KerningPairs[j-1].Right;
    v2:=(KerningTables[i].KerningPairs[j].Left shl 16) or KerningTables[i].KerningPairs[j].Right;
    if v1>=v2 then begin
     KerningTables[i].BinarySearch:=false;
     break;
    end;
   end;
  end;
  Pos:=Next;
 end;

 LastError:=TT_ERR_NoError;
 result:=LastError;
end;

function TBeRoVectorCanvasTrueTypeFont.ProcessCharacterMappingTable:integer;
var pos,tag,checksum,offset,size,numsubtables,thisPlatformID,thisSpecificID,thisSubtableOffset,subtableOffset,i:longword;
    subtableLength:integer;
    subtablefound:boolean;
begin
 tag:=toLONGWORD(byte('c'),byte('m'),byte('a'),byte('p'));
 result:=GetTableDirEntry(Tag,checksum,offset,size);
 if result<>TT_Err_NoError then exit;
 pos:=offset;
 inc(pos,sizeof(word)); // Table Version number
 numsubtables:=toWORD(fontData[pos],fontData[pos+1]);
 inc(pos,sizeof(word));

 subtablefound:=false;
 SubtableOffset:=0;
 i:=0;
 while (i<numsubtables) and not subtablefound do begin
  thisPlatformID:=toWORD(fontData[pos],fontData[pos+1]);
  inc(pos,sizeof(word));
  thisSpecificID:=toWORD(fontData[pos],fontData[pos+1]);
  inc(pos,sizeof(word));
  thisSubtableOffset:=toLONGWORD(fontData[pos],fontData[pos+1],fontData[pos+2],fontData[pos+3]);
  inc(pos,sizeof(longword));
  if (thisPlatformID=PlatformID) and (thisSpecificID=SpecificID) then begin
   subtablefound:=true;
   SubtableOffset:=thisSubtableOffset;
   break;
  end;
  inc(i);
 end;

 if not subtablefound then begin
  LastError:=TT_ERR_NoCharacterMapFound;
  result:=LastError;
  exit;
 end;

 pos:=offset+subtableOffset;

 cmapFormat:=toWORD(fontData[pos],fontData[pos+1]);

 case cmapFormat of
  CMAP_FORMAT0,CMAP_FORMAT2,CMAP_FORMAT4,CMAP_FORMAT6:begin
   subtableLength:=toWORD(fontData[pos+2],fontData[pos+3]);
  end;
  CMAP_FORMAT8,CMAP_FORMAT10,CMAP_FORMAT12,CMAP_FORMAT13:begin
   subtableLength:=toLONGWORD(fontData[pos+4],fontData[pos+5],fontData[pos+6],fontData[pos+7]);
  end;
  else begin
   LastError:=TT_ERR_UnknownCharacterMapFormat;
   result:=LastError;
   exit;
  end;
 end;

 setlength(cmap,subtableLength);
 if length(cmap)<>subtableLength then begin
  LastError:=TT_ERR_OutOfMemory;
  result:=LastError;
  exit;
 end;
 move(fontData[offset+subtableOffset],cmap[0],subtableLength);

 LastError:=TT_ERR_NoError;
 result:=LastError;
end;

function TBeRoVectorCanvasTrueTypeFont.GetGlyphIndex(CharCode:longword):word;
var segcount,u:word;
    endCount,startcount,idDelta,idRangeOffset,data,subHeaderKeysData:PByteArray;
    endv,start,delta,range,index,seg,i,numSH,numGlyphId,index1,idx,IDDeltaValue,
    IDRangeOffsetValue,offset,FirstCode,EntryCount,l,h,m,CharHi,CharLo:longword;
begin
 LastError:=TT_ERR_NoError;
 if length(cmap)=0 then begin
  LastError:=TT_ERR_CharacterMapNotPresent;
  result:=0;
  exit;
 end;
 case cmapformat of
  CMAP_FORMAT0:begin
   offset:=CharCode+6;
   if offset<longword(length(cmap)) then begin
    result:=cmap[offset];
   end else begin
    result:=0;
   end;
  end;
  CMAP_FORMAT2:begin
   if CharCode>$ffff then begin
    result:=0;
   end else begin
    if CharCode<256 then begin
     index:=CharCode;
    end else begin
     index:=CharCode shr 8;
    end;
    subHeaderKeysData:=@cmap[6];
    numSH:=0;
    for i:=0 to 255 do begin
     u:=toWORD(subHeaderKeysData[i*2],subHeaderKeysData[(i*2)+1]) shr 3;
     subHeaderKeys[i]:=u;
     if numSH<u then begin
      numSH:=u;
     end;
    end;
    numGlyphId:=((longword(length(cmap))-(2*(256+3))-(numSH*8)) and $ffff) div 2;
    index1:=subHeaderKeys[index];
    if index1=0 then begin
     if CharCode<256 then begin
      if CharCode<numGlyphId then begin
       data:=@cmap[6+(256*2)+(numSH*8)];
       result:=toWORD(data^[CharCode*2],data^[(CharCode*2)+1]);
      end else begin
       result:=0;
      end;
     end else begin
      result:=0;
     end;
    end else begin
     if CharCode<256 then begin
      result:=0;
     end else begin
      idx:=CharCode and $ff;
      data:=@cmap[6+(256*2)+(index1*8)];
      FirstCode:=toWORD(data^[0],data^[1]);
      EntryCount:=toWORD(data^[2],data^[3]);
      IDDeltaValue:=toWORD(data^[4],data^[5]);
      IDRangeOffsetValue:=toWORD(data^[6],data^[7])-(((NumSH-index1)*8)+2);
      if (idx>=FirstCode) and (idx<(FirstCode+EntryCount)) then begin
       offset:=(IDRangeOffsetValue div 2)+(idx-FirstCode);
       if offset<numGlyphId then begin
        data:=@cmap[6+(256*2)+(numSH*8)];
        result:=toWORD(data^[offset*2],data^[(offset*2)+1]);
        if result<>0 then begin
         result:=(result+IDDeltaValue) and $ffff;
        end;
       end else begin
        result:=0;
       end;
      end else begin
       result:=0;
      end;
     end;
    end;
   end;
  end;
  CMAP_FORMAT4:begin
   if CharCode>$ffff then begin
    result:=0;
   end else begin
    CharCode:=CharCode and $ffff;
    segCount:=toWORD(cmap[6],cmap[7]) shr 1;
    endCount:=@cmap[14];
    startCount:=@cmap[16+(2*segCount)];
    idDelta:=@cmap[16+(4*segCount)];
    idRangeOffset:=@cmap[16+(6*segCount)];
    seg:=0;
    endv:=toWORD(endCount^[0],endCount^[1]);
    while endv<CharCode do begin
     inc(seg);
     endv:=toWORD(endCount^[seg*2],endCount^[(seg*2)+1]);
    end;
    start:=toWORD(startCount^[seg*2],startCount^[(seg*2)+1]);
    delta:=toWORD(idDelta^[seg*2],idDelta^[(seg*2)+1]);
    range:=toWORD(idRangeOffset[seg*2],idRangeOffset[(seg*2)+1]);
    if start>CharCode then begin
     result:=0;
    end else begin
     if range=0 then begin
      index:=(word(CharCode)+word(delta)) and $ffff;
     end else begin
      index:=range+((CharCode-start)*2)+((16+(6*segCount))+(seg*2));
      index:=toWORD(cmap[index],cmap[index+1]);
      if index<>0 then begin
       index:=(word(index)+word(delta)) and $ffff;
      end;
     end;
     result:=index;
    end;
   end;
  end;
  CMAP_FORMAT6:begin
   FirstCode:=toWORD(cmap[6],cmap[7]);
   EntryCount:=toWORD(cmap[8],cmap[9]);
   if (CharCode>=FirstCode) and (CharCode<(FirstCode+EntryCount)) then begin
    offset:=(longword(CharCode-FirstCode)*2)+10;
    result:=toWORD(cmap[offset],cmap[offset+1]);
   end else begin
    result:=0;
   end;
  end;
  CMAP_FORMAT8:begin
   result:=0;
   EntryCount:=toLONGWORD(cmap[8204],cmap[8205],cmap[8206],cmap[8207]);
   if EntryCount>0 then begin
    l:=0;
    h:=EntryCount;
    while l<h do begin
     m:=(l+h) shr 1;
     offset:=8208+(m*12);
     start:=toLONGWORD(cmap[offset],cmap[offset+1],cmap[offset+2],cmap[offset+3]);
     endv:=toLONGWORD(cmap[offset+4],cmap[offset+5],cmap[offset+6],cmap[offset+7]);
     if CharCode<start then begin
      h:=m-1;
     end else if CharCode>endv then begin
      l:=m+1;
     end else begin
      if (m and $ffff0000)=0 then begin
       if (cmap[12+(m shr 3)] and ($80 shr (m and 7)))<>0 then begin
        break;
       end;
      end else begin
       if ((cmap[12+((m and $ffff) shr 3)] and ($80 shr ((m and $ffff) and 7)))=0) or
           ((cmap[12+(((m shr 16) and $ffff) shr 3)] and ($80 shr (((m shr 16) and $ffff) and 7)))=0) then begin
         break;
       end;
      end;
      result:=(toLONGWORD(cmap[offset+8],cmap[offset+9],cmap[offset+10],cmap[offset+11])+CharCode)-start;
      break;
     end;
    end;
   end;
  end;
  CMAP_FORMAT10:begin
   FirstCode:=toLONGWORD(cmap[12],cmap[13],cmap[14],cmap[15]);
   EntryCount:=toLONGWORD(cmap[16],cmap[17],cmap[18],cmap[19]);
   if (CharCode>=FirstCode) and (CharCode<(FirstCode+EntryCount)) then begin
    offset:=(longword(CharCode-FirstCode)*2)+20;
    result:=toWORD(cmap[offset],cmap[offset+1]);
   end else begin
    result:=0;
   end;
  end;
  CMAP_FORMAT12:begin
   result:=0;
   EntryCount:=toWORD(cmap[6],cmap[7]);
   if EntryCount>0 then begin
    l:=0;
    h:=EntryCount;
    while l<h do begin
     m:=(l+h) shr 1;
     offset:=16+(m*12);
     start:=toLONGWORD(cmap[offset],cmap[offset+1],cmap[offset+2],cmap[offset+3]);
     endv:=toLONGWORD(cmap[offset+4],cmap[offset+5],cmap[offset+6],cmap[offset+7]);
     if CharCode<start then begin
      h:=m-1;
     end else if CharCode>endv then begin
      l:=m+1;
     end else begin
      result:=(toLONGWORD(cmap[offset+8],cmap[offset+9],cmap[offset+10],cmap[offset+11])+CharCode)-start;
      break;
     end;
    end;
   end;
  end;
  CMAP_FORMAT13:begin
   result:=0;
   EntryCount:=toWORD(cmap[6],cmap[7]);
   if EntryCount>0 then begin
    l:=0;
    h:=EntryCount;
    while l<h do begin
     m:=(l+h) shr 1;
     offset:=16+(m*12);
     start:=toLONGWORD(cmap[offset],cmap[offset+1],cmap[offset+2],cmap[offset+3]);
     endv:=toLONGWORD(cmap[offset+4],cmap[offset+5],cmap[offset+6],cmap[offset+7]);
     if CharCode<start then begin
      h:=m-1;
     end else if CharCode>endv then begin
      l:=m+1;
     end else begin
      result:=toLONGWORD(cmap[offset+8],cmap[offset+9],cmap[offset+10],cmap[offset+11]);
      break;
     end;
    end;
   end;
  end;
  else begin
   result:=0;
  end;
 end;
end;

function TBeRoVectorCanvasTrueTypeFont.numGlyphs:integer;
begin
 result:=length(Glyphs);
end;

function TBeRoVectorCanvasTrueTypeFont.Glyph(Code:longword):PBeRoVectorCanvasTrueTypeGlyph;
var i:integer;
begin
 if length(Glyphs)<>0 then begin
  i:=GetGlyphIndex(Code);
  if (i>=0) and (i<length(Glyphs)) then begin
   result:=@Glyphs[i];
  end else begin
   result:=nil;
  end;
 end else begin
  result:=nil;
 end;
end;

function TBeRoVectorCanvasTrueTypeFont.Kerning(Left,Right:word;Horizontal:boolean):smallint;
var i,j,beg,mid,endv,idx:integer;
    thiscombined,combined:longword;
begin
 result:=0;
 if length(KerningTables)>0 then begin
  for i:=0 to length(KerningTables)-1 do begin
   if (length(KerningTables[i].KerningPairs)<>0) and (KerningTables[i].Horizontal=Horizontal) then begin
    combined:=(Left shl 16) or Right;
    idx:=-1;
    if KerningTables[i].BinarySearch then begin
     beg:=0;
     endv:=length(KerningTables[i].KerningPairs);
     while beg<=endv do begin
      mid:=(beg+endv) div 2;
      thisCombined:=(KerningTables[i].KerningPairs[mid].left shl 16) or KerningTables[i].KerningPairs[mid].right;
      if combined=thisCombined then begin
       idx:=mid;
       break;
      end;
      if combined<thisCombined then begin
       endv:=mid-1;
      end else begin
       beg:=mid+1;
      end;
     end;
    end else begin
     for j:=0 to length(KerningTables[i].KerningPairs)-1 do begin
      thisCombined:=(KerningTables[i].KerningPairs[j].left shl 16) or KerningTables[i].KerningPairs[j].right;
      if combined=thisCombined then begin
       idx:=j;
       break;
      end;
     end;
    end;
    if idx>=0 then begin
     if KerningTables[i].ValueOverride then begin
      result:=KerningTables[i].KerningPairs[idx].value;
     end else begin
      inc(result,KerningTables[i].KerningPairs[idx].value);
     end;
    end;
   end;
  end;
 end;
end;

function TBeRoVectorCanvasTrueTypeFont.CharacterMap(c:longword):word;
begin
 result:=GetGlyphIndex(c);
end;

function TBeRoVectorCanvasTrueTypeFont.CharacterMap(c:widechar):word;
begin
 result:=GetGlyphIndex(word(c));
end;

function TBeRoVectorCanvasTrueTypeFont.NumContours(GlyphNum:word):smallint;
begin
 if GlyphNum<length(Glyphs) then begin
  result:=length(Glyphs[glyphnum].Contours);
 end else begin
  result:=0;
 end;
end;

function TBeRoVectorCanvasTrueTypeFont.NumPoints(GlyphNum,ContourNum:word):smallint;
begin
 if (GlyphNum<length(Glyphs)) and (ContourNum<length(Glyphs[glyphnum].Contours)) then begin
  result:=length(Glyphs[glyphnum].Contours[ContourNum].Points);
 end else begin
  result:=0;
 end;
end;

function TBeRoVectorCanvasTrueTypeFont.PointX(GlyphNum,ContourNum,PointNum:word):integer;
begin
 if (GlyphNum<length(Glyphs)) and (ContourNum<length(Glyphs[glyphnum].Contours)) and (PointNum<length(Glyphs[glyphnum].Contours[ContourNum].Points)) then begin
  result:=Glyphs[glyphnum].Contours[ContourNum].Points[PointNum].x;
 end else begin
  result:=0;
 end;
end;

function TBeRoVectorCanvasTrueTypeFont.PointY(GlyphNum,ContourNum,PointNum:word):integer;
begin
 if (GlyphNum<length(Glyphs)) and (ContourNum<length(Glyphs[glyphnum].Contours)) and (PointNum<length(Glyphs[glyphnum].Contours[ContourNum].Points)) then begin
  result:=Glyphs[glyphnum].Contours[ContourNum].Points[PointNum].y;
 end else begin
  result:=0;
 end;
end;

function TBeRoVectorCanvasTrueTypeFont.OnCurve(GlyphNum,ContourNum,PointNum:word):boolean;
begin
 if (GlyphNum<length(Glyphs)) and (ContourNum<length(Glyphs[glyphnum].Contours)) and (PointNum<length(Glyphs[glyphnum].Contours[ContourNum].Points)) then begin
  result:=Glyphs[glyphnum].Contours[ContourNum].Points[PointNum].OnCurve;
 end else begin
  result:=false;
 end;
end;

function TBeRoVectorCanvasTrueTypeFont.GlyphXMin(GlyphNum:word):smallint;
begin
 if GlyphNum<length(Glyphs) then begin
  result:=Glyphs[glyphnum].XMin;
 end else begin
  result:=0;
 end;
end;

function TBeRoVectorCanvasTrueTypeFont.GlyphYMin(GlyphNum:word):smallint;
begin
 if GlyphNum<length(Glyphs) then begin
  result:=Glyphs[glyphnum].YMin;
 end else begin
  result:=0;
 end;
end;

function TBeRoVectorCanvasTrueTypeFont.GlyphXMax(GlyphNum:word):smallint;
begin
 if GlyphNum<length(Glyphs) then begin
  result:=Glyphs[glyphnum].XMax;
 end else begin
  result:=0;
 end;
end;

function TBeRoVectorCanvasTrueTypeFont.GlyphYMax(GlyphNum:word):smallint;
begin
 if GlyphNum<length(Glyphs) then begin
  result:=Glyphs[glyphnum].YMax;
 end else begin
  result:=0;
 end;
end;

constructor TBeRoVectorCanvasShape.Create;
begin
 inherited Create;
 Commands:=nil;
 NumCommands:=0;
 cells:=nil;
 numcells:=0;
 spans:=nil;
 numspans:=0;
 UDDcells:=nil;
 NumUDDCells:=0;
 UDDLines:=nil;
 NumUDDLines:=0;
 ScanLines:=nil;
end;

destructor TBeRoVectorCanvasShape.Destroy;
var i:integer;
begin
 setlength(Commands,0);
 for i:=0 to length(cells)-1 do begin
  if assigned(cells[i]) then begin
   dispose(cells[i]);
   cells[i]:=nil;
  end;
 end;
 setlength(cells,0);
 for i:=0 to length(UDDcells)-1 do begin
  if assigned(UDDcells[i]) then begin
   dispose(UDDcells[i]);
   UDDcells[i]:=nil;
  end;
 end;
 setlength(UDDcells,0);
 for i:=0 to length(spans)-1 do begin
  if assigned(spans[i]) then begin
   dispose(spans[i]);
   spans[i]:=nil;
  end;
 end;
 setlength(UDDLines,0);
 setlength(spans,0);
 for i:=0 to length(scanlines)-1 do begin
  setlength(scanlines[i].cells,0);
  setlength(scanlines[i].spans,0);
  setlength(scanlines[i].UDDcells,0);
 end;
 setlength(scanlines,0);
 inherited Destroy;
end;

constructor TBeRoVectorCanvas.Create;
begin
 inherited Create;
 DefaultShape:=TBeRoVectorCanvasShape.Create;
 CurrentShape:=DefaultShape;
 SaveCommands:=CurrentShape<>DefaultShape;
 ClearUDDBuffer:=false;
 UDDExtraPixels:=4;
 CurrentGamma:=0;
 SetGamma(1);
 Matrix:=MatrixIdentity;
 IsExternalCanvas:=false;
 PolyPoints:=nil;
 NumPolyPoints:=0;
 DraftEdges:=nil;
 CurrentCanvas:=nil;
 BufferCanvasUDD:=nil;
 BufferCanvas:=nil;
 CustomColorProc:=nil;
 CustomColorInstance:=nil;
 CurrentColor:=$000000;
 CurrentWinding:=false;
 CurrentWidth:=0;
 CurrentHeight:=0;
 CurrentStyleMode:=bvcsmFILL;
 CurrentLineCapMode:=bvclcmBUTT;
 CurrentLineWidth:=256;
 CurrentLineJoinMode:=bvcljmBEVEL;
 CurrentLineInnerJoinMode:=bvclijmBEVEL;
 LinePoints:=nil;
 NumLinePoints:=0;
 LineStrokePattern:='';
 LineStrokePatternStepSize:=256;
 LineStrokePatternNewLine:=false;
 CurrentHandleUDD:=false;
 MatrixStack:=nil;
 MatrixStackPosition:=-1;
 ShapeStack:=nil;
 ShapeStackPosition:=-1;
 lastcx:=0;
 lastcy:=0;
 lastx:=0;
 lasty:=0;
 lasta:=0;
 lastb:=0;
 lastc:=0;
 lastd:=0;
 laste:=0;
 lastf:=0;
 mx:=0;
 my:=0;
 ma:=0;
 mb:=0;
 mc:=0;
 md:=0;
 me:=0;
 mf:=0;
 lx:=0;
 ly:=0;
 FlushLineOnWork:=false;
end;

destructor TBeRoVectorCanvas.Destroy;
var i:integer;
begin
 for i:=0 to length(PolyPoints)-1 do begin
  setlength(PolyPoints[i].Points,0);
 end;
 setlength(PolyPoints,0);
 for i:=0 to length(DraftEdges)-1 do begin
  if assigned(DraftEdges[i]) then begin
   freemem(DraftEdges[i]);
   DraftEdges[i]:=nil;
  end;
 end;
 setlength(DraftEdges,0);
 if assigned(BufferCanvasUDD) then begin
  freemem(BufferCanvasUDD);
  BufferCanvasUDD:=nil;
 end;
 if assigned(BufferCanvas) then begin
  freemem(BufferCanvas);
  BufferCanvas:=nil;
 end;
 if assigned(CurrentCanvas) and not IsExternalCanvas then begin
  freemem(CurrentCanvas);
  CurrentCanvas:=nil;
 end;
 setlength(MatrixStack,0);
 for i:=0 to length(ShapeStack)-1 do begin
  if assigned(ShapeStack[i]) then begin
   if ShapeStack[i]<>DefaultShape then begin
    ShapeStack[i].Destroy;
   end;
   ShapeStack[i]:=nil;
  end;
 end;
 setlength(ShapeStack,0);
 if assigned(DefaultShape) then begin
  DefaultShape.Destroy;
  DefaultShape:=nil;
 end;
 inherited Destroy;
end;

function TBeRoVectorCanvas.Canvas:pointer;
begin
 result:=CurrentCanvas;
end;

procedure TBeRoVectorCanvas.UseExternalCanvas(DstCanvas:pointer);
begin
 if assigned(CurrentCanvas) and not IsExternalCanvas then begin
  freemem(CurrentCanvas);
  CurrentCanvas:=nil;
 end;
 CurrentCanvas:=DstCanvas;
 IsExternalCanvas:=true;
end;

procedure TBeRoVectorCanvas.ResetShape;
var i:integer;
begin
 CurrentShape.numcells:=0;
 CurrentShape.numspans:=0;
 CurrentShape.numuddcells:=0;
 CurrentShape.NumUDDLines:=0;
 for i:=0 to length(CurrentShape.scanlines)-1 do begin
  CurrentShape.scanlines[i].numcells:=0;
  CurrentShape.scanlines[i].numspans:=0;
  CurrentShape.scanlines[i].numUDDcells:=0;
 end;
end;

procedure TBeRoVectorCanvas.ResetLocal;
begin
 NumLinePoints:=0;
 NumPolyPoints:=0;
 area:=0;
 cover:=0;
 ex:=-1;
 ey:=-1;
 renderminy:=0;
 rendermaxy:=0;
 renderminmaxfirst:=true;
 NeedToClose:=false;
end;

procedure TBeRoVectorCanvas.Reset;
begin
 ResetShape;
 ResetLocal;
end;

procedure TBeRoVectorCanvas.Setup(NewWidth,NewHeight:integer;NewRenderingMode:TBeRoVectorCanvasRenderingMode);
begin
 if (CurrentWidth<>NewWidth) or (CurrentHeight<>NewHeight) or (CurrentRenderingMode<>NewRenderingMode) or (not assigned(BufferCanvas)) or (not assigned(BufferCanvasUDD)) then begin
  CurrentWidth:=NewWidth;
  CurrentHeight:=NewHeight;
  CurrentRenderingMode:=NewRenderingMode;
  case CurrentRenderingMode of
   bvcrmDRAFT,bvcrmCOLORINTENSITY:begin
    CurrentWidthEx:=CurrentWidth;
   end;
   bvcrmRGB,bvcrmBGR:begin
    CurrentWidthEx:=CurrentWidth*3;
   end;
  end;
  if assigned(BufferCanvasUDD) then begin
   freemem(BufferCanvasUDD);
   BufferCanvasUDD:=nil;
  end;
  if assigned(BufferCanvas) then begin
   freemem(BufferCanvas);
   BufferCanvas:=nil;
  end;
  getmem(BufferCanvasUDD,CurrentWidthEx*CurrentHeight*sizeof(TBeRoVectorCanvasUDDCell));
  getmem(BufferCanvas,CurrentWidthEx*CurrentHeight*sizeof(longword));
  if not IsExternalCanvas then begin
   if assigned(CurrentCanvas) then begin
    freemem(CurrentCanvas);
    CurrentCanvas:=nil;
   end;
   getmem(CurrentCanvas,CurrentWidth*CurrentHeight*sizeof(longword));
  end;
 end;
 CheckScanLinesArray;
end;

procedure TBeRoVectorCanvas.CheckScanLinesArray;
var i:integer;
begin
 if length(CurrentShape.scanlines)<>CurrentHeight then begin
  for i:=0 to length(CurrentShape.scanlines)-1 do begin
   setlength(CurrentShape.scanlines[i].cells,0);
   setlength(CurrentShape.scanlines[i].spans,0);
   setlength(CurrentShape.scanlines[i].UDDcells,0);
  end;
  setlength(CurrentShape.scanlines,CurrentHeight);
  for i:=0 to length(CurrentShape.scanlines)-1 do begin
   CurrentShape.scanlines[i].cells:=nil;
   CurrentShape.scanlines[i].numcells:=0;
   CurrentShape.scanlines[i].spans:=nil;
   CurrentShape.scanlines[i].numspans:=0;
   CurrentShape.scanlines[i].UDDcells:=nil;
   CurrentShape.scanlines[i].numUDDcells:=0;
  end;
 end;
end;

function TBeRoVectorCanvas.NewCell:PBeRoVectorCanvasCell;
var oi,ni,i:integer;
begin
 CheckScanLinesArray;
 if (ey<0) or (ey>=length(CurrentShape.scanlines)) then begin
  result:=nil;
  exit;
 end;
 if CurrentShape.numcells>=length(CurrentShape.cells) then begin
  oi:=length(CurrentShape.cells);
  ni:=((CurrentShape.numcells+1)+memoryinc) and not memoryincmask;
  setlength(CurrentShape.cells,ni);
  for i:=oi to ni-1 do begin
   new(CurrentShape.cells[i]);
  end;
 end;
 result:=CurrentShape.cells[CurrentShape.numcells];
 inc(CurrentShape.numcells);
 result^.x:=ex;
 result^.area:=0;
 result^.cover:=0;
 if renderminmaxfirst then begin
  renderminmaxfirst:=false;
  renderminy:=ey;
  rendermaxy:=ey;
 end else begin
  if renderminy>ey then begin
   renderminy:=ey;
  end;
  if rendermaxy<ey then begin
   rendermaxy:=ey;
  end;
 end;
 if CurrentShape.scanlines[ey].numcells>=length(CurrentShape.scanlines[ey].cells) then begin
  setlength(CurrentShape.scanlines[ey].cells,((CurrentShape.scanlines[ey].numcells+1)+memoryinc) and not memoryincmask);
 end;
 CurrentShape.scanlines[ey].cells[CurrentShape.scanlines[ey].numcells]:=result;
 inc(CurrentShape.scanlines[ey].numcells);
end;

procedure TBeRoVectorCanvas.RecordCell;
var c:PBeRoVectorCanvasCell;
begin
 if (area<>0) or (cover<>0) then begin
  c:=NewCell;
  if assigned(c) then begin
   inc(c^.area,area);
   inc(c^.cover,cover);
  end;
 end;
end;

procedure TBeRoVectorCanvas.SetCell(nex,ney:integer;force:boolean=false);
begin
 if (ex<>nex) or (ey<>ney) or force then begin
  RecordCell;
  ex:=nex;
  ey:=ney;
  lastx:=ex;
  lasty:=ey;
  area:=0;
  cover:=0;
 end;
end;

procedure TBeRoVectorCanvas.StartCell(nex,ney:integer);
begin
 if CurrentRenderingMode=bvcrmDRAFT then begin
  if NumPolyPoints>=length(PolyPoints) then begin
   setlength(PolyPoints,((NumPolyPoints+1)+memoryinc) and not memoryincmask);
  end;
  PolyPoints[NumPolyPoints].NumPoints:=0;
  if PolyPoints[NumPolyPoints].NumPoints>=length(PolyPoints[NumPolyPoints].Points) then begin
   setlength(PolyPoints[NumPolyPoints].Points,((PolyPoints[NumPolyPoints].NumPoints+1)+memoryinc) and not memoryincmask);
  end;
  PolyPoints[NumPolyPoints].Points[PolyPoints[NumPolyPoints].NumPoints].x:=nex;
  PolyPoints[NumPolyPoints].Points[PolyPoints[NumPolyPoints].NumPoints].y:=ney;
  inc(PolyPoints[NumPolyPoints].NumPoints);
  inc(NumPolyPoints);
 end;
 NeedToClose:=true;
 area:=0;
 cover:=0;
 SetCell(nex,ney,true);
end;

function TBeRoVectorCanvas.interpolateUDD(p,a1,a2,b1,b2:integer):integer;
var bd:integer;
begin
 if not CurrentHandleUDD then begin
  result:=0;
  exit;
 end;
 bd:=b2-b1;
 if bd=0 then begin
  result:=a1;
  exit
 end;
 result:=a1+(((a2-a1)*(p-b1)) div bd);
 if a1<a2 then begin
  if result<a1 then begin
   result:=a1;
  end else if result>a2 then begin
   result:=a2;
  end;
 end else begin
  if result<a2 then begin
   result:=a2;
  end else if result>a1 then begin
   result:=a1;
  end;
 end;
end;

procedure TBeRoVectorCanvas.RenderScanLine(ney,x1,y1,x2,y2:integer);
var ex1,ex2,fx1,fx2,delta,p,first,dx,incr,lift,imod,rem:integer;
begin
 dx:=x2-x1;
 ex1:=x1 div pixelfactor;
 ex2:=x2 div pixelfactor;
 fx1:=x1 mod pixelfactor;
 fx2:=x2 mod pixelfactor;

 if y1=y2 then begin
  // A trivial case. Happens often
  SetCell(ex2,ney);
  exit;
 end;
 
 if ex1=ex2 then begin
  // Everything is located in a single x-based cell. That is easy!
  delta:=y2-y1;
  inc(area,(fx1+fx2)*delta);
  inc(cover,delta);
  exit;
 end;
 
 // Ok, we'll have to render a run of adjacent cells on the same scanline.
 if dx<0 then begin
  p:=fx1*(y2-y1);
  first:=0;
  incr:=-1;
  dx:=-dx;
 end else begin
  p:=(onepixel-fx1)*(y2-y1);
  first:=onepixel;
  incr:=1;
 end;

 delta:=p div dx;
 imod:=p mod dx;
 if imod<0 then begin
  dec(delta);
  inc(imod,dx);
 end;
 
 inc(area,(fx1+first)*delta);
 inc(cover,delta);
 
 inc(ex1,incr);
 SetCell(ex1,ney);
 inc(y1,delta);

 if ex1<>ex2 then begin
  p:=onepixel*((y2-y1)+delta);
  lift:=p div dx;
  rem:=p mod dx;
  if rem<0 then begin
   dec(lift);
   inc(rem,dx);
  end;
  dec(imod,dx);
  while ex1<>ex2 do begin
   delta:=lift;
   inc(imod,rem);
   if imod>=0 then begin
    dec(imod,dx);
    inc(delta);
   end;
   inc(area,onepixel*delta);
   inc(cover,delta);
   inc(y1,delta);
   inc(ex1,incr);
   SetCell(ex1,ney);
  end;
 end;
 delta:=y2-y1;
 inc(area,(fx2+onepixel-first)*delta);
 inc(cover,delta);
end;

procedure TBeRoVectorCanvas.RenderLine(tox,toy:integer;toa:integer=0;tob:integer=0;toc:integer=0;tod:integer=0;toe:integer=0;tof:integer=0);
var ey1,ey2,fy1,fy2,dx,dy,nx,x2,p,first,delta,rem,imod,lift,incr,min,max,nex,twofx,narea:integer;
begin
 if HandleUDD then begin
  if CurrentShape.NumUDDLines>=length(CurrentShape.UDDLines) then begin
   setlength(CurrentShape.UDDLines,((CurrentShape.NumUDDLines+1)+memoryinc) and not memoryincmask);
  end;
  CurrentShape.UDDLines[CurrentShape.NumUDDLines].x1:=lastx;
  CurrentShape.UDDLines[CurrentShape.NumUDDLines].y1:=lasty;
  CurrentShape.UDDLines[CurrentShape.NumUDDLines].x2:=tox;
  CurrentShape.UDDLines[CurrentShape.NumUDDLines].y2:=toy;
  CurrentShape.UDDLines[CurrentShape.NumUDDLines].a1:=lasta;
  CurrentShape.UDDLines[CurrentShape.NumUDDLines].b1:=lastb;
  CurrentShape.UDDLines[CurrentShape.NumUDDLines].c1:=lastc;
  CurrentShape.UDDLines[CurrentShape.NumUDDLines].d1:=lastd;
  CurrentShape.UDDLines[CurrentShape.NumUDDLines].e1:=laste;
  CurrentShape.UDDLines[CurrentShape.NumUDDLines].f1:=lastf;
  CurrentShape.UDDLines[CurrentShape.NumUDDLines].a2:=toa;
  CurrentShape.UDDLines[CurrentShape.NumUDDLines].b2:=tob;
  CurrentShape.UDDLines[CurrentShape.NumUDDLines].c2:=toc;
  CurrentShape.UDDLines[CurrentShape.NumUDDLines].d2:=tod;
  CurrentShape.UDDLines[CurrentShape.NumUDDLines].e2:=toe;
  CurrentShape.UDDLines[CurrentShape.NumUDDLines].f2:=tof;
  inc(CurrentShape.NumUDDLines);
 end;
 if CurrentRenderingMode=bvcrmDRAFT then begin
  if NumPolyPoints=0 then begin
   inc(NumPolyPoints);
  end;
  if NumPolyPoints>0 then begin
   if (NumPolyPoints-1)>=length(PolyPoints) then begin
    setlength(PolyPoints,(((NumPolyPoints-1)+1)+memoryinc) and not memoryincmask);
   end;
   if PolyPoints[(NumPolyPoints-1)].NumPoints>=length(PolyPoints[(NumPolyPoints-1)].Points) then begin
    setlength(PolyPoints[(NumPolyPoints-1)].Points,((PolyPoints[(NumPolyPoints-1)].NumPoints+1)+memoryinc) and not memoryincmask);
   end;
   PolyPoints[(NumPolyPoints-1)].Points[PolyPoints[(NumPolyPoints-1)].NumPoints].x:=tox div pixelfactor;
   PolyPoints[(NumPolyPoints-1)].Points[PolyPoints[(NumPolyPoints-1)].NumPoints].y:=toy div pixelfactor;
   inc(PolyPoints[(NumPolyPoints-1)].NumPoints);
  end;
  lastx:=tox;
  lasty:=toy;
  lasta:=toa;
  lastb:=tob;
  lastc:=toc;
  lastd:=tod;
  laste:=toe;
  lastf:=tof;
  exit;
 end;
 ey1:=lasty div pixelfactor;
 ey2:=toy div pixelfactor;
 fy1:=lasty mod pixelfactor;
 fy2:=toy mod pixelfactor;
 dx:=tox-lastx;
 dy:=toy-lasty;

 // Perform vertical clipping
 if ey1<ey2 then begin
  min:=ey1;
  max:=ey2;
 end else begin
  min:=ey2;
  max:=ey1;
 end;
 if (min>=Currentheight) or (max<0) then begin
  lastx:=tox;
  lasty:=toy;
  lasta:=toa;
  lastb:=tob;
  lastc:=toc;
  lastd:=tod;
  laste:=toe;
  lastf:=tof;
  exit;
 end;
 
 if ey1=ey2 then begin
  // Everything is on a single scanline
  RenderScanLine(ey1,lastx,fy1,tox,fy2);
  lastx:=tox;
  lasty:=toy;
  lasta:=toa;
  lastb:=tob;
  lastc:=toc;
  lastd:=tod;
  laste:=toe;
  lastf:=tof;
  exit;
 end;
 
 // Vertical line - avoid calling RenderScanLine
 incr:=1;
 if dx=0 then begin
  nex:=lastx div pixelfactor;
  twofx:=(lastx mod pixelfactor)*2;

  first:=onepixel;
  if dy<0 then begin
   first:=0;
   incr:=-1;
  end;

  delta:=first-fy1;
  inc(area,twofx*delta);
  inc(cover,delta);
  inc(ey1,incr);

  SetCell(nex,ey1);

  delta:=first+first-onepixel;
  narea:=twofx*delta;
  while ey1<>ey2 do begin
   inc(area,narea);
   inc(cover,delta);
   inc(ey1,incr);
   SetCell(nex,ey1);
  end;

  delta:=fy2-onepixel+first;
  inc(area,twofx*delta);
  inc(cover,delta);
  lastx:=tox;
  lasty:=toy;
  lasta:=toa;
  lastb:=tob;
  lastc:=toc;
  lastd:=tod;
  laste:=toe;
  lastf:=tof;
  exit;
 end;
 
 // Ok, we have to render several scanlines
 if dy<0 then begin
  p:=fy1*dx;
  first:=0;
  incr:=-1;
  dy:=-dy;
 end else begin
  p:=(onepixel-fy1)*dx;
  first:=onepixel;
  incr:=1;
 end;
 
 delta:=p div dy;
 imod:=p mod dy;
 if imod<0 then begin
  dec(delta);
  inc(imod,dy);
 end;
 
 nx:=lastx+delta;
 RenderScanLine(ey1,lastx,fy1,nx,first);

 inc(ey1,incr);
 SetCell(nx div pixelfactor,ey1);

 if ey1<>ey2 then begin
  p:=onepixel*dx;
  lift:=p div dy;
  rem:=p mod dy;
  if rem<0 then begin
   dec(lift);
   inc(rem,dy);
  end;
  while ey1<>ey2 do begin
   delta:=lift;
   inc(imod,rem);
   if imod>=0 then begin
    dec(imod,dy);
    inc(delta);
   end;
   x2:=nx+delta;
   RenderScanLine(ey1,nx,onepixel-first,x2,first);
   nx:=x2;
   inc(ey1,incr);
   SetCell(nx div pixelfactor,ey1);
  end;
 end;
 RenderScanLine(ey1,nx,onepixel-first,tox,fy2);
 
 lastx:=tox;
 lasty:=toy;
 lasta:=toa;
 lastb:=tob;
 lastc:=toc;
 lastd:=tod;
 laste:=toe;
 lastf:=tof;
end;

procedure TBeRoVectorCanvas.RenderSpanDRAFT(y:integer;span:PBeRoVectorCanvasSpan);
var p:^longword;
    pUDD:PBeRoVectorCanvasUDDCell;
    i:integer;
    a,c:longword;
begin
 if span^.coverage<>0 then begin
  p:=CurrentCanvas;
  inc(p,(y*CurrentWidthEx)+span^.x);
  if assigned(CustomColorProc) then begin
   if CurrentHandleUDD then begin
    pUDD:=BufferCanvasUDD;
    inc(pUDD,(y*CurrentWidthEx)+span^.x);
    for i:=0 to span^.len-1 do begin
     c:=CustomColorProc(CustomColorInstance,span^.x+i,y,pUDD^.a,pUDD^.b,pUDD^.c,pUDD^.d,pUDD^.e,pUDD^.f);
     a:=c shr 24;
     p^:=blend(p^,c,a);
     inc(p);
     inc(pUDD);
    end;
   end else begin
    for i:=0 to span^.len-1 do begin
     c:=CustomColorProc (CustomColorInstance,span^.x+i,y,0,0,0,0,0,0);
     a:=c shr 24;
     p^:=blend(p^,c,a);
     inc(p);
    end;
   end;
  end else begin
   a:=CurrentColor shr 24;
   if a=255 then begin
    for i:=1 to span^.len do begin
     p^:=(CurrentColor and $00ffffff) or $ff000000;
     inc(p);
    end;
   end else if a<>0 then begin
    for i:=1 to span^.len do begin
     p^:=blend(p^,CurrentColor,a);
     inc(p);
    end;
   end;
  end;
 end;
end;

procedure TBeRoVectorCanvas.RenderSpanCOLORINTENSITY(y:integer;span:PBeRoVectorCanvasSpan);
var p:^longword;
    pUDD:PBeRoVectorCanvasUDDCell;
    i:integer;
    a,c:longword;
begin
 if span^.coverage<>0 then begin
  p:=CurrentCanvas;
  inc(p,(y*CurrentWidthEx)+span^.x);
  if assigned(CustomColorProc) then begin
   if CurrentHandleUDD then begin
    pUDD:=BufferCanvasUDD;
    inc(pUDD,(y*CurrentWidthEx)+span^.x);
    for i:=0 to span^.len-1 do begin
     c:=CustomColorProc(CustomColorInstance,span^.x+i,y,pUDD^.a,pUDD^.b,pUDD^.c,pUDD^.d,pUDD^.e,pUDD^.f);
     a:=(longword(GammaLookUpTable[span^.coverage and $ff])*(c shr 24)) shr 8;
     p^:=blend(p^,c,a);
     inc(p);
     inc(pUDD);
    end;
   end else begin
    for i:=0 to span^.len-1 do begin
     c:=CustomColorProc (CustomColorInstance,span^.x+i,y,0,0,0,0,0,0);
     a:=(longword(GammaLookUpTable[span^.coverage and $ff])*(c shr 24)) shr 8;
     p^:=blend(p^,c,a);
     inc(p);
    end;
   end;
  end else begin
   a:=(longword(GammaLookUpTable[span^.coverage and $ff])*(CurrentColor shr 24)) shr 8;
   if a=255 then begin
    for i:=1 to span^.len do begin
     p^:=CurrentColor or $ff000000;
     inc(p);
    end;
   end else if a<>0 then begin
    for i:=1 to span^.len do begin
     p^:=blend(p^,CurrentColor,a);
     inc(p);
    end;
   end;
  end;
 end;
end;

procedure TBeRoVectorCanvas.RenderSpanRGBBGR(y:integer;span:PBeRoVectorCanvasSpan);
var p:^byte;
    c:byte;
begin
 if span^.coverage<>0 then begin
  p:=buffercanvas;
  inc(p,(y*CurrentWidthEx)+span^.x);
  c:=GammaLookUpTable[span^.coverage and $ff];
  if c<>0 then begin
   fillchar(p^,span^.len,c);
  end;
 end;
end;

procedure TBeRoVectorCanvas.AddSpan(x,y,area,acount:integer);
var span:PBeRoVectorCanvasSpan;
    oi,ni,i,coverage:integer;
begin
 coverage:=abs(area div (1 shl ((pixelbits*2)+1-8)));
 if CurrentWinding then begin
  if coverage>255 then begin
   coverage:=255;
  end;
 end else begin
  coverage:=coverage and 511;
  if coverage>256 then begin
   coverage:=512-coverage;
  end else if coverage=256 then begin
   coverage:=255;
  end;
 end;
 if coverage<>0 then begin
  if CurrentShape.numspans>=length(CurrentShape.spans) then begin
   oi:=length(CurrentShape.spans);
   ni:=((CurrentShape.numspans+1)+memoryinc) and not memoryincmask;
   setlength(CurrentShape.spans,ni);
   for i:=oi to ni-1 do begin
    new(CurrentShape.spans[i]);
   end;
  end;
  span:=CurrentShape.spans[CurrentShape.numspans];
  span^.x:=x;
  span^.len:=acount;
  span^.coverage:=coverage;
  inc(CurrentShape.numspans);
  if CurrentShape.scanlines[y].numspans>=length(CurrentShape.scanlines[y].spans) then begin
   setlength(CurrentShape.scanlines[y].spans,((CurrentShape.scanlines[y].numspans+1)+memoryinc) and not memoryincmask);
  end;
  CurrentShape.scanlines[y].spans[CurrentShape.scanlines[y].numspans]:=span;
  inc(CurrentShape.scanlines[y].numspans);
 end;
end;

procedure TBeRoVectorCanvas.GetDraftMinMaxY;
var i,j:integer;
    firsty:boolean;
begin
 firsty:=true;
 for i:=0 to NumPolyPoints-1 do begin
  for j:=0 to PolyPoints[i].NumPoints-1 do begin
   if firsty then begin
    firsty:=false;
    renderminy:=PolyPoints[i].Points[j].y;
    rendermaxy:=PolyPoints[i].Points[j].y;
   end else begin
    if renderminy>PolyPoints[i].Points[j].y then begin
     renderminy:=PolyPoints[i].Points[j].y;
    end;
    if rendermaxy<PolyPoints[i].Points[j].y then begin
     rendermaxy:=PolyPoints[i].Points[j].y;
    end;
   end;
  end;
 end;
 if renderminy<0 then begin
  renderminy:=0;
 end else if renderminy>=CurrentHeight then begin
  renderminy:=CurrentHeight-1;
 end;
 if rendermaxy<0 then begin
  rendermaxy:=0;
 end else if rendermaxy>=CurrentHeight then begin
  rendermaxy:=CurrentHeight-1;
 end;
end;

procedure TBeRoVectorCanvas.MakeDraftSpans;
var i,j,y,EdgeCount:integer;
 procedure MakeEdge(var Edge:TBeRoVectorCanvasDraftEdge;const frompoint,topoint:TBeRoVectorCanvasPolyPoint);
 begin
  fillchar(Edge,sizeof(TBeRoVectorCanvasDraftEdge),#0);
  if topoint.y<frompoint.y then begin
   Edge.fromx:=topoint.x;
   Edge.fromy:=topoint.y;
   Edge.tox:=frompoint.x;
   Edge.toy:=frompoint.y;
   Edge.ydir:=-1;
   Edge.error:=-1;
  end else begin
   Edge.fromx:=frompoint.x;
   Edge.fromy:=frompoint.y;
   Edge.tox:=topoint.x;
   Edge.toy:=topoint.y;
   Edge.ydir:=1;
   Edge.error:=0;
  end;
  edge.x:=edge.fromx;
  edge.y:=edge.fromy;
  edge.dx:=edge.tox-edge.fromx;
  edge.dy:=edge.toy-edge.fromy;
  edge.absdx:=abs(edge.dx);
  edge.absdy:=abs(edge.dy);
  edge.xmajor:=edge.absdx>edge.absdy;
  if edge.xmajor then begin
   edge.errormax:=edge.absdx;
   edge.erroradj:=(edge.absdy shl 16) div edge.absdx;
  end else if edge.absdy<>0 then begin
   edge.errormax:=edge.absdy;
   edge.erroradj:=(edge.absdx shl 16) div edge.absdy;
  end;
  edge.erroracc:=0;
  inc(edge.error,edge.errormax div 2);
  if edge.dx<0 then begin
   edge.xdir:=-1;
  end else begin
   edge.xdir:=1;
  end;
  edge.next:=nil;
 end;
 function BuildActiveEdgeList(y:integer):PBeRoVectorCanvasDraftEdge;
 var i:integer;
  procedure UpdateScanLine(var edge:TBeRoVectorCanvasDraftEdge;y:integer);
  var s,x1:integer;
  begin
   assert((edge.fromy<=y) and (edge.toy>y));
   if edge.xmajor then begin
    assert(edge.y=y);
    s:=(edge.errormax-edge.error-1) div edge.absdy;
    if s<>0 then begin
     x1:=edge.x;
     inc(edge.x,s*edge.xdir);
     inc(edge.error,s*edge.absdy);
     inc(edge.erroracc,s*edge.erroradj);
     assert(edge.error<edge.errormax);
     if x1<edge.x then begin
      edge.xcoord[0]:=x1;
     end else begin
      edge.xcoord[0]:=edge.x;
     end;
     if x1>edge.x then begin
      edge.xcoord[1]:=x1;
     end else begin
      edge.xcoord[1]:=edge.x;
     end;
    end else begin
     edge.xcoord[0]:=edge.x;
     edge.xcoord[1]:=edge.x;
    end;
    inc(edge.x,edge.xdir);
    assert(((edge.errorMax-edge.error-1) div edge.absdy)=0);
    inc(edge.error,edge.absdy);
    inc(edge.erroracc,edge.erroradj);
    assert(edge.error>=edge.errormax);
    dec(edge.error,edge.absdx);
    inc(edge.y);
   end else begin
    edge.xcoord[0]:=edge.x;
    edge.xcoord[1]:=edge.x;
    inc(edge.error,edge.absdx);
    inc(edge.erroracc,edge.erroradj);
    inc(edge.y);
    if edge.error>=edge.errormax then begin
     dec(edge.error,edge.errormax);
     inc(edge.x,edge.xdir);
     assert(edge.error<edge.errormax);
    end;
   end;
  end;
  procedure InsertToActiveEdgeList(var activehead:PBeRoVectorCanvasDraftEdge;newedge:PBeRoVectorCanvasDraftEdge);
  var previous,edge:PBeRoVectorCanvasDraftEdge;
   function edgecompare(const a,b:TBeRoVectorCanvasDraftEdge):integer;
   begin
    result:=(a.xcoord[0]+a.xcoord[1])-(b.xcoord[0]+b.xcoord[1]);
   end;
  begin
   if not assigned(activehead) then begin
    newedge^.next:=nil;
    activehead:=newedge;
   end else if edgecompare(newedge^,activehead^)<=0 then begin
    newedge^.next:=activehead;
    activehead:=newedge;
   end else begin
    previous:=nil;
    edge:=activehead;
    while assigned(edge) and (edgecompare(edge^,newedge^)<0) do begin
     previous:=edge;
     edge:=edge^.next;
    end;
    assert(assigned(previous));
    newedge^.next:=previous^.next;
    previous^.next:=newedge;
   end;
  end;
 begin
  result:=nil;
  for i:=0 to EdgeCount-1 do begin
   if (DraftEdges[i]^.fromy<=y) and (DraftEdges[i]^.toy>y) then begin
    UpdateScanLine(DraftEdges[i]^,y);
    InsertToActiveEdgeList(result,DraftEdges[i]);
   end;
  end;
 end;
 procedure AddScanLineSpan(y,x1,x2:integer);
 var span:PBeRoVectorCanvasSpan;
     i,oi,ni:integer;
 begin                   
  if (y<renderminy) or (y>rendermaxy) then exit;
  if CurrentShape.numspans>=length(CurrentShape.spans) then begin
   oi:=length(CurrentShape.spans);
   ni:=((CurrentShape.numspans+1)+memoryinc) and not memoryincmask;
   setlength(CurrentShape.spans,ni);
   for i:=oi to ni-1 do begin
    new(CurrentShape.spans[i]);
   end;
  end;
  span:=CurrentShape.spans[CurrentShape.numspans];
  if x1<=x2 then begin
   span^.x:=x1;
   span^.len:=x2-x1+1;
  end else begin
   // This should be never happen!
   span^.x:=x2;
   span^.len:=x1-x2+1;
  end;
  span^.coverage:=255;
  inc(CurrentShape.numspans);
  if CurrentShape.scanlines[y].numspans>=length(CurrentShape.scanlines[y].spans) then begin
   setlength(CurrentShape.scanlines[y].spans,((CurrentShape.scanlines[y].numspans+1)+memoryinc) and not memoryincmask);
  end;
  CurrentShape.scanlines[y].spans[CurrentShape.scanlines[y].numspans]:=span;
  inc(CurrentShape.scanlines[y].numspans);
 end;
 procedure RenderScanLineAlternative(y:integer;activehead:PBeRoVectorCanvasDraftEdge);
 var left,right:PBeRoVectorCanvasDraftEdge;
 begin
  if not assigned(activehead) then exit;
  left:=activehead;
  right:=left^.next;
  assert(assigned(right));
  while assigned(right) do begin
   AddScanLineSpan(y,left^.xcoord[0],right^.xcoord[1]);
   left:=right^.next;
   if assigned(left) then begin
    right:=left^.next;
   end else begin
    break;
   end;
  end;
 end;
 procedure RenderScanLineWinding(y:integer;activehead:PBeRoVectorCanvasDraftEdge);
 var left,right:PBeRoVectorCanvasDraftEdge;
     x1,x2,nx1,nx2,winding:integer;
 begin
  if not assigned(activehead) then exit;
  left:=activehead;
  right:=left^.next;
  winding:=left^.ydir;
  assert(assigned(right));
  x1:=left^.xcoord[0];
  x2:=right^.xcoord[1];
  left:=right;
  right:=left^.next;
  inc(winding,left^.ydir);
  while assigned(right) do begin
   nx1:=left^.xcoord[0];
   nx2:=right^.xcoord[1];
   if winding<>0 then begin
    if ((nx1>=x1) and (nx1<=x2)) or ((nx2>=x1) and (nx2<=x2)) or ((x1>=nx1) and (x1<=nx2)) or ((x2>=nx2) and (x2<=nx2)) then begin
     if nx1<x1 then begin
      x1:=nx1;
     end;
     if nx2>x2 then begin
      x2:=nx2;
     end;
    end else begin
     AddScanLineSpan(y,x1,x2);
     x1:=nx1;
     x2:=nx2;
    end;
   end;
   left:=right;
   right:=left^.next;
   inc(winding,left^.ydir);
  end;
  AddScanLineSpan(y,x1,x2);
 end;
 procedure RenderScanLine(y:integer;activehead:PBeRoVectorCanvasDraftEdge);
 begin
  if CurrentWinding then begin
   RenderScanLineWinding(y,activehead);
  end else begin
   RenderScanLineAlternative(y,activehead);
  end;
 end;
 procedure UpdateEdgeMemory;
 var i,oi,ni:integer;
 begin
  if EdgeCount>=length(DraftEdges) then begin
   oi:=length(DraftEdges);
   ni:=((EdgeCount+1)+memoryinc) and not memoryincmask;
   setlength(DraftEdges,ni);
   for i:=oi to ni-1 do begin
    new(DraftEdges[i]);
   end;
  end;
 end;
begin
 EdgeCount:=0;
 for i:=0 to NumPolyPoints-1 do begin
  if PolyPoints[i].NumPoints<3 then continue;
  for j:=1 to PolyPoints[i].NumPoints-1 do begin
   UpdateEdgeMemory;
   MakeEdge(DraftEdges[EdgeCount]^,PolyPoints[i].Points[j-1],PolyPoints[i].Points[j]);
   if DraftEdges[EdgeCount]^.absdy<>0 then begin
    inc(EdgeCount);
   end;
  end;
  if (PolyPoints[i].Points[PolyPoints[i].NumPoints-1].x<>PolyPoints[i].Points[0].x) or (PolyPoints[i].Points[PolyPoints[i].NumPoints-1].y<>PolyPoints[i].Points[0].y) then begin
   UpdateEdgeMemory;
   MakeEdge(DraftEdges[EdgeCount]^,PolyPoints[i].Points[PolyPoints[i].NumPoints-1],PolyPoints[i].Points[0]);
   if DraftEdges[EdgeCount]^.absdy<>0 then begin
    inc(EdgeCount);
   end;
  end;
 end;
 if EdgeCount=0 then exit;
 for y:=renderminy to rendermaxy do begin
  RenderScanLine(y,BuildActiveEdgeList(y));
 end;
end;

procedure TBeRoVectorCanvas.SortScanLineCells;
var i:integer;
 procedure QuickSort(var ScanLine:TBeRoVectorCanvasScanLine;l,r:integer);
 var i,j:integer;
     p,t:PBeRoVectorCanvasCell;
 begin
  repeat
   i:=l;
   j:=r;
   p:=ScanLine.cells[(l+r) div 2];
   repeat
    while ScanLine.cells[i]^.x<p^.x do inc(i);
    while ScanLine.cells[j]^.x>p^.x do dec(j);
    if i<=j then begin
     t:=ScanLine.cells[i];
     ScanLine.cells[i]:=ScanLine.cells[j];
     ScanLine.cells[j]:=t;
     inc(i);
     dec(j);
    end;
   until i>j;
   if l<j then begin
    QuickSort(ScanLine,l,j);
   end;
   l:=i;
  until i>=r;
 end;
 procedure BeRoSort(var ScanLine:TBeRoVectorCanvasScanLine);
 var i:integer;
     t:PBeRoVectorCanvasCell;
 begin
  i:=0;
  while i<(ScanLine.numcells-1) do begin
   if ScanLine.cells[i]^.x>ScanLine.cells[i+1]^.x then begin
    t:=ScanLine.cells[i];
    ScanLine.cells[i]:=ScanLine.cells[i+1];
    ScanLine.cells[i+1]:=t;
    if i>0 then begin
     dec(i);
    end else begin
     inc(i);
    end;
   end else begin
    inc(i);
   end;
  end;
 end;
begin
 for i:=renderminy to rendermaxy do begin
  if CurrentShape.scanlines[i].numcells<2 then continue;
  if CurrentShape.scanlines[i].numcells>2 then begin
   QuickSort(CurrentShape.scanlines[i],0,CurrentShape.scanlines[i].numcells-1);
  end;
  BeRoSort(CurrentShape.scanlines[i]);
 end;
end;

procedure TBeRoVectorCanvas.OptimizeScanLineCells;
var i,j,k:integer;
begin
 for i:=renderminy to rendermaxy do begin
  j:=1;
  while j<CurrentShape.scanlines[i].numcells do begin
   if CurrentShape.scanlines[i].cells[j]^.x=CurrentShape.scanlines[i].cells[j-1]^.x then begin
    inc(CurrentShape.scanlines[i].cells[j-1]^.area,CurrentShape.scanlines[i].cells[j]^.area);
    inc(CurrentShape.scanlines[i].cells[j-1]^.cover,CurrentShape.scanlines[i].cells[j]^.cover);
    k:=CurrentShape.scanlines[i].numcells-(j+1);
    if k>0 then begin
     move(CurrentShape.scanlines[i].cells[j+1],CurrentShape.scanlines[i].cells[j],k*sizeof(PBeRoVectorCanvasCell));
    end;
    dec(CurrentShape.scanlines[i].numcells);
   end else begin
    inc(j);
   end;
  end;
 end;
end;

procedure TBeRoVectorCanvas.MakeScanLineSpans;
var cover,x,area,i,j:integer;
    c:PBeRoVectorCanvasCell;
begin
 if CurrentShape.numcells=0 then exit;
 CurrentShape.numspans:=0;
 for i:=renderminy to rendermaxy do begin
  x:=0;
  cover:=0;
  CurrentShape.scanlines[i].numspans:=0;
  for j:=0 to CurrentShape.scanlines[i].numcells-1 do begin
   c:=CurrentShape.scanlines[i].cells[j];
   if (c^.x>x) and (cover<>0) then begin
    AddSpan(x,i,cover*(onepixel*2),c^.x-x);
   end;
   inc(cover,c^.cover);
   area:=(cover*(onepixel*2))-c^.area;
   if (area<>0) and (c^.x>=0) then begin
    AddSpan(c^.x,i,area,1);
   end;
   x:=c^.x+1;
  end;
 end;
end;

procedure TBeRoVectorCanvas.SortScanLineSpans;
var i:integer;
 procedure QuickSort(var ScanLine:TBeRoVectorCanvasScanLine;l,r:integer);
 var i,j:integer;
     p,t:PBeRoVectorCanvasSpan;
 begin
  repeat
   i:=l;
   j:=r;
   p:=ScanLine.spans[(l+r) div 2];
   repeat
    while ScanLine.spans[i]^.x<p^.x do inc(i);
    while ScanLine.spans[j]^.x>p^.x do dec(j);
    if i<=j then begin
     t:=ScanLine.spans[i];
     ScanLine.spans[i]:=ScanLine.spans[j];
     ScanLine.spans[j]:=t;
     inc(i);
     dec(j);
    end;
   until i>j;
   if l<j then begin
    QuickSort(ScanLine,l,j);
   end;
   l:=i;
  until i>=r;
 end;
 procedure BeRoSort(var ScanLine:TBeRoVectorCanvasScanLine);
 var i:integer;
     t:PBeRoVectorCanvasSpan;
 begin
  i:=0;
  while i<(ScanLine.numspans-1) do begin
   if ScanLine.spans[i]^.x>ScanLine.spans[i+1]^.x then begin
    t:=ScanLine.spans[i];
    ScanLine.spans[i]:=ScanLine.spans[i+1];
    ScanLine.spans[i+1]:=t;
    if i>0 then begin
     dec(i);
    end else begin
     inc(i);
    end;
   end else begin
    inc(i);
   end;
  end;
 end;
begin
 for i:=renderminy to rendermaxy do begin
  if CurrentShape.scanlines[i].numspans<2 then continue;
  if CurrentShape.scanlines[i].numspans>2 then begin
   QuickSort(CurrentShape.scanlines[i],0,CurrentShape.scanlines[i].numspans-1);
  end;
  BeRoSort(CurrentShape.scanlines[i]);
 end;
end;

procedure TBeRoVectorCanvas.ClipScanLineSpans;
var i,j,k,x,l:integer;
    span:PBeRoVectorCanvasSpan;
begin
 for i:=renderminy to rendermaxy do begin
  j:=0;
  while j<CurrentShape.scanlines[i].numspans do begin
   span:=CurrentShape.scanlines[i].spans[j];
   x:=span^.x;
   l:=span^.len;
   if ((x+l)<=0) or (x>=CurrentWidthEx) then begin
    l:=0;
   end else begin
    if x<0 then begin
     inc(l,x);
     if l<0 then begin
      l:=0;
     end else begin
      x:=0;
     end;
    end;
    if (x+l-1)>=CurrentWidthEx then begin
     l:=CurrentWidthEx-x;
    end;
   end;
   if l=0 then begin
    k:=CurrentShape.scanlines[i].numspans-(j+1);
    if k>0 then begin
     move(CurrentShape.scanlines[i].spans[j+1],CurrentShape.scanlines[i].spans[j],k*sizeof(PBeRoVectorCanvasspan));
    end;
    dec(CurrentShape.scanlines[i].numspans);
   end else begin
    span^.x:=x;
    span^.len:=l;
    inc(j);
   end;
  end;
 end;
end;

procedure TBeRoVectorCanvas.OptimizeScanLineSpans;
var i,j,k:integer;
begin
 for i:=renderminy to rendermaxy do begin
  j:=1;
  while j<CurrentShape.scanlines[i].numspans do begin
   if ((CurrentShape.scanlines[i].spans[j-1]^.x+CurrentShape.scanlines[i].spans[j-1]^.len)=CurrentShape.scanlines[i].spans[j]^.x) and (CurrentShape.scanlines[i].spans[j]^.coverage=CurrentShape.scanlines[i].spans[j-1]^.coverage) then begin
    inc(CurrentShape.scanlines[i].spans[j-1]^.len,CurrentShape.scanlines[i].spans[j]^.len);
    k:=CurrentShape.scanlines[i].numspans-(j+1);
    if k>0 then begin
     move(CurrentShape.scanlines[i].spans[j+1],CurrentShape.scanlines[i].spans[j],k*sizeof(PBeRoVectorCanvasspan));
    end;
    dec(CurrentShape.scanlines[i].numspans);
   end else begin
    inc(j);
   end;
  end;
 end;
end;

procedure TBeRoVectorCanvas.RenderScanLineSpans;
var i,j:integer;
begin
 for i:=renderminy to rendermaxy do begin
  for j:=0 to CurrentShape.scanlines[i].numspans-1 do begin
   case CurrentRenderingMode of
    bvcrmDRAFT:begin
     RenderSpanDRAFT(i,CurrentShape.scanlines[i].spans[j]);
    end;
    bvcrmCOLORINTENSITY:begin
     RenderSpanCOLORINTENSITY(i,CurrentShape.scanlines[i].spans[j]);
    end;
    bvcrmRGB,bvcrmBGR:begin
     RenderSpanRGBBGR(i,CurrentShape.scanlines[i].spans[j]);
    end;
   end;
  end;
 end;
end;

procedure TBeRoVectorCanvas.FilterSubpixelBufferCanvas;
const coefs:array[0..4] of longword=($10,$40,$70,$40,$10);
var x,y:integer;
    fir:array[0..4] of longword;
    v1,v,pv:longword;
    p:^byte;
    pa:pbytearray absolute p;
begin
 if (CurrentWidthEx<3) or (CurrentHeight<1) then exit;
 p:=buffercanvas;
 inc(p,CurrentWidthEx*renderminy);
 for y:=renderminy to rendermaxy do begin
  v1:=pa^[0];
  fir[0]:=coefs[2]*v1;
  fir[1]:=coefs[3]*v1;
  fir[2]:=coefs[4]*v1;
  fir[3]:=0;
  fir[4]:=0;
  v1:=pa^[1];
  inc(fir[0],coefs[1]*v1);
  inc(fir[1],coefs[2]*v1);
  inc(fir[2],coefs[3]*v1);
  inc(fir[3],coefs[4]*v1);
  for x:=2 to CurrentWidthEx-1 do begin
   v:=pa^[x];
   pv:=fir[0]+(coefs[0]*v);
   fir[0]:=fir[1]+(coefs[1]*v);
   fir[1]:=fir[2]+(coefs[2]*v);
   fir[2]:=fir[3]+(coefs[3]*v);
   fir[3]:=coefs[4]*v;
   pv:=pv shr 8;
   pv:=pv or (-(pv shr 8));
   pa^[x-2]:=pv and $ff;
  end;
  pv:=fir[0] shr 8;
  pv:=pv or (-(pv shr 8));
  pa^[CurrentWidthEx-2]:=pv and $ff;
  pv:=fir[1] shr 8;
  pv:=pv or (-(pv shr 8));
  pa^[CurrentWidthEx-1]:=pv and $ff;
  inc(p,CurrentWidthEx);
 end;
end;

procedure TBeRoVectorCanvas.TranslateRGBBufferCanvas;
var x,y,sx:integer;
    c,d:array[0..2] of longword;
    sp:^byte;
    dp:^longword;
    a,sa,ca,cv:longword;
    pUDD:PBeRoVectorCanvasUDDCell;
begin
 FilterSubpixelBufferCanvas;
 cv:=CurrentColor;
 c[0]:=cv and $ff;
 c[1]:=(cv shr 8) and $ff;
 c[2]:=(cv shr 16) and $ff;
 ca:=cv shr 24;
 pUDD:=BufferCanvasUDD;
 inc(pUDD,CurrentWidthEx*renderminy);
 sp:=BufferCanvas;
 inc(sp,CurrentWidthEx*renderminy);
 dp:=CurrentCanvas;
 inc(dp,CurrentWidth*renderminy);
 for y:=renderminy to rendermaxy do begin
  for x:=0 to CurrentWidth-1 do begin
   if assigned(CustomColorProc) and not CurrentHandleUDD then begin
    cv:=CustomColorProc(CustomColorInstance,x,y,0,0,0,0,0,0);
    c[0]:=cv and $ff;
    c[1]:=(cv shr 8) and $ff;
    c[2]:=(cv shr 16) and $ff;
    ca:=cv shr 24;
   end;
   d[0]:=dp^ and $ff;
   d[1]:=(dp^ shr 8) and $ff;
   d[2]:=(dp^ shr 16) and $ff;
   sa:=0;
   for sx:=2 downto 0 do begin
    if assigned(CustomColorProc) and CurrentHandleUDD then begin
     cv:=CustomColorProc(CustomColorInstance,x,y,pUDD^.a,pUDD^.b,pUDD^.c,pUDD^.d,pUDD^.e,pUDD^.f);
     c[0]:=cv and $ff;
     c[1]:=(cv shr 8) and $ff;
     c[2]:=(cv shr 16) and $ff;
     ca:=cv shr 24;
    end;
    a:=(sp^*ca) shr 8;
    if a<>0 then begin
     d[sx]:=((((c[sx]-d[sx])*a)+(d[sx]*256)) div 256) and $ff;
    end;
    inc(sa,a);
    inc(sp);
    if CurrentHandleUDD then begin
     inc(pUDD);
    end;
   end;
{$ifdef cpu386}
   asm
    push eax
    push edx
    mov eax,$0AAAAAAAB
    mul dword ptr sa
    shr edx,1
    and edx,$ff
    mov dword ptr sa,edx
    pop edx
    pop eax
   end;
{$else}
   sa:=(sa div 3) and $ff;
{$endif}
   dp^:=d[0] or (d[1] shl 8) or (d[2] shl 16) or (sa shl 24);
   inc(dp);
  end;
 end;
end;

procedure TBeRoVectorCanvas.TranslateBGRBufferCanvas;
var x,y,sx:integer;
    c,d:array[0..2] of longword;
    sp:^byte;
    dp:^longword;
    a,sa,ca,cv:longword;
    pUDD:PBeRoVectorCanvasUDDCell;
begin
 FilterSubpixelBufferCanvas;
 cv:=CurrentColor;
 c[0]:=cv and $ff;
 c[1]:=(cv shr 8) and $ff;
 c[2]:=(cv shr 16) and $ff;
 ca:=cv shr 24;
 pUDD:=BufferCanvasUDD;
 inc(pUDD,CurrentWidthEx*renderminy);
 sp:=BufferCanvas;
 inc(sp,CurrentWidthEx*renderminy);
 dp:=CurrentCanvas;
 inc(dp,CurrentWidth*renderminy);
 for y:=renderminy to rendermaxy do begin
  for x:=0 to CurrentWidth-1 do begin
   if assigned(CustomColorProc) and not CurrentHandleUDD then begin
    cv:=CustomColorProc(CustomColorInstance,x,y,0,0,0,0,0,0);
    c[0]:=cv and $ff;
    c[1]:=(cv shr 8) and $ff;
    c[2]:=(cv shr 16) and $ff;
    ca:=cv shr 24;
   end;
   d[0]:=dp^ and $ff;
   d[1]:=(dp^ shr 8) and $ff;
   d[2]:=(dp^ shr 16) and $ff;
   sa:=0;
   for sx:=0 to 2 do begin
    if assigned(CustomColorProc) and CurrentHandleUDD then begin
     cv:=CustomColorProc(CustomColorInstance,x,y,pUDD^.a,pUDD^.b,pUDD^.c,pUDD^.d,pUDD^.e,pUDD^.f);
     c[0]:=cv and $ff;
     c[1]:=(cv shr 8) and $ff;
     c[2]:=(cv shr 16) and $ff;
     ca:=cv shr 24;
    end;
    a:=(sp^*ca) shr 8;
    if a<>0 then begin
     d[sx]:=((((c[sx]-d[sx])*a)+(d[sx]*256)) div 256) and $ff;
    end;
    inc(sa,a);
    inc(sp);
    if CurrentHandleUDD then begin
     inc(pUDD);
    end;
   end;
{$ifdef cpu386}
   asm
    push eax
    push edx
    mov eax,$0AAAAAAAB
    mul dword ptr sa
    shr edx,1
    and edx,$ff
    mov dword ptr sa,edx
    pop edx
    pop eax
   end;
{$else}
   sa:=(sa div 3) and $ff;
{$endif}
   dp^:=d[0] or (d[1] shl 8) or (d[2] shl 16) or (sa shl 24);
   inc(dp);
  end;
 end;
end;

procedure TBeRoVectorCanvas.Clear(AColor:longword=$ffffff);
var p:^longword;
{$IFDEF cpu386}
    pixels:integer;
{$ELSE}
    x,y:integer;
{$ENDIF}
begin
 if assigned(CurrentCanvas) then begin
  p:=CurrentCanvas;
{$IFDEF cpu386}
  pixels:=CurrentWidth*CurrentHeight;
  asm
   pushad
    cld
    mov eax,dword ptr AColor
    mov ecx,dword ptr pixels
    mov edi,dword ptr p
    rep stosd
   popad
  end;
{$ELSE}
  for y:=1 to Height do begin
   for x:=1 to Width do begin
    p^:=Color;
    inc(p);
   end;
  end;
{$ENDIF}
 end;
 if ClearUDDBuffer and assigned(BufferCanvasUDD) then begin
  fillchar(BufferCanvasUDD^,CurrentWidthEx*CurrentHeight*sizeof(TBeRoVectorCanvasUDDCell),#0);
 end;
 lastx:=0;
 lasty:=0;
 lasta:=0;
 lastb:=0;
 lastc:=0;
 lastd:=0;
 laste:=0;
 lastf:=0;
 mx:=0;
 my:=0;
 ma:=0;
 mb:=0;
 mc:=0;
 md:=0;
 me:=0;
 mf:=0;
 lx:=0;
 ly:=0;
end;

procedure TBeRoVectorCanvas.FlushLine;
var StepCounter,StepIndex,
    StepEndX,StepEndY,StepEndA,StepEndB,StepEndC,StepEndD,StepEndE,StepEndF:integer;
    NewLine,LastStepBool,StepBool,DoFlushStepLine,DoConvertLineStorkeToPolygon:boolean;
    LinePointsBuf:TBeRoVectorCanvasLinePoints;
    i:integer;
 procedure AddLinePointEx(x,y,a,b,c,d,e,f:integer);
 begin
  if (NumLinePoints=0) or ((NumLinePoints>0) and ((LinePoints[NumLinePoints-1].x<>x) or (LinePoints[NumLinePoints-1].y<>y))) then begin
   AddLinePoint(x,y,a,b,c,d,e,f);
   DoConvertLineStorkeToPolygon:=true;
  end;
 end;
 procedure FlushStepLine;
 begin
  if DoConvertLineStorkeToPolygon then begin
   if NumLinePoints>1 then begin
    DoFlushStepLine:=false;
    DoConvertLineStorkeToPolygon:=false;
    ConvertLineStorkeToPolygon;
    NumLinePoints:=0;
   end;
  end;
 end;
 procedure StepPoint(x,y,a,b,c,d,e,f:integer);
 begin
  if StepCounter=0 then begin
   if StepIndex>length(LineStrokePattern) then begin
    StepIndex:=1;
   end;
   LastStepBool:=StepBool;
   if StepIndex<=length(LineStrokePattern) then begin
    StepBool:=LineStrokePattern[StepIndex]<>' ';
   end else begin
    StepBool:=false;
   end;
   if StepBool then begin
    DoFlushStepLine:=true;
    if not LastStepBool then begin
     AddLinePointEx(x,y,a,b,c,d,e,f);
    end;
   end else begin
    FlushStepLine;
    DoFlushStepLine:=false;
   end;
  end else if NewLine then begin
   if StepBool then begin
    AddLinePointEx(x,y,a,b,c,d,e,f);
   end;
  end;
  if StepBool then begin
   StepEndX:=x;
   StepEndY:=y;
   StepEndA:=a;
   StepEndB:=b;
   StepEndC:=c;
   StepEndD:=d;
   StepEndE:=e;
   StepEndF:=f;
  end;
  inc(StepCounter);
  if StepCounter>=LineStrokePatternStepSize then begin
   if StepBool then begin
    AddLinePointEx(x,y,a,b,c,d,e,f);
   end;
   StepCounter:=0;
   inc(StepIndex);
  end;
  NewLine:=false;
 end;
 function DoScale(ps,pc,pe,vs,ve:integer):integer;
 begin
  if ps=pe then begin
   result:=vs;
  end else begin
   if ps<pe then begin
    result:=vs+(((ve-vs)*(pc-ps)) div (pe-ps));
   end else begin
    result:=vs+(((ve-vs)*(pc-pe)) div (ps-pe));
   end;
  end;
 end;
 procedure StepLine(x0,y0,a0,b0,c0,d0,e0,f0,x1,y1,a1,b1,c1,d1,e1,f1:integer);
 var stepx,stepy,xs,ys,dx,dy,fraction:integer;
 begin
  NewLine:=LineStrokePatternNewLine;
  dx:=x1-x0;
  dy:=y1-y0;
  if dx<0 then begin
   dx:=-dx;
   stepx:=-1;
  end else begin
   stepx:=1;
  end;
  if dy<0 then begin
   dy:=-dy;
   stepy:=-1;
  end else begin
   stepy:=1;
  end;
  xs:=x0;
  ys:=y0;
  dx:=dx*2;
  dy:=dy*2;
  StepPoint(x0,y0,a0,b0,c0,d0,e0,f0);
  if dx>dy then begin
   fraction:=dy-(dx div 2);
   while x0<>x1 do begin
    if fraction>=0 then begin
     inc(y0,stepy);
     dec(fraction,dx);
    end;
    inc(x0,stepx);
    inc(fraction,dy);
    StepPoint(x0,y0,DoScale(xs,x0,x1,a0,a1),DoScale(xs,x0,x1,b0,b1),DoScale(xs,x0,x1,c0,c1),DoScale(xs,x0,x1,d0,d1),DoScale(xs,x0,x1,e0,e1),DoScale(xs,x0,x1,f0,f1));
   end;
  end else begin
   fraction:=dx-(dy div 2);
   while y0<>y1 do begin
    if fraction>=0 then begin
     inc(x0,stepx);
     dec(fraction,dy);
    end;
    inc(y0,stepy);
    inc(fraction,dx);
    StepPoint(x0,y0,DoScale(ys,y0,y1,a0,a1),DoScale(ys,y0,y1,b0,b1),DoScale(ys,y0,y1,c0,c1),DoScale(ys,y0,y1,d0,d1),DoScale(ys,y0,y1,e0,e1),DoScale(ys,y0,y1,f0,f1));
   end;
  end;
 end;
begin
 if FlushLineOnWork then exit;
 FlushLineOnWork:=true;
 if NumLinePoints>0 then begin
  if length(LineStrokePattern)>0 then begin
   StepCounter:=0;
   StepIndex:=1;
   DoFlushStepLine:=false;
   DoConvertLineStorkeToPolygon:=false;
   setlength(LinePointsBuf,NumLinePoints);
   for i:=0 to NumLinePoints-1 do begin
    LinePointsBuf[i]:=LinePoints[i];
   end;
   LastStepBool:=false;
   StepBool:=false;
   NumLinePoints:=0;
   if length(LinePointsBuf)=0 then begin
    StepLine(LinePointsBuf[0].x,LinePointsBuf[0].y,LinePointsBuf[0].UDDa,LinePointsBuf[0].UDDb,LinePointsBuf[0].UDDc,LinePointsBuf[0].UDDd,LinePointsBuf[0].UDDe,LinePointsBuf[0].UDDf,LinePointsBuf[0].x,LinePointsBuf[0].y,LinePointsBuf[0].UDDa,LinePointsBuf[0].UDDb,LinePointsBuf[0].UDDc,LinePointsBuf[0].UDDd,LinePointsBuf[0].UDDe,LinePointsBuf[0].UDDf);
   end else begin
    for i:=1 to length(LinePointsBuf)-1 do begin
     StepLine(LinePointsBuf[i-1].x,LinePointsBuf[i-1].y,LinePointsBuf[i-1].UDDa,LinePointsBuf[i-1].UDDb,LinePointsBuf[i-1].UDDc,LinePointsBuf[i-1].UDDd,LinePointsBuf[i-1].UDDe,LinePointsBuf[i-1].UDDf,LinePointsBuf[i].x,LinePointsBuf[i].y,LinePointsBuf[i].UDDa,LinePointsBuf[i].UDDb,LinePointsBuf[i].UDDc,LinePointsBuf[i].UDDd,LinePointsBuf[i].UDDe,LinePointsBuf[i].UDDf);
    end;
   end;
   if StepBool and DoFlushStepLine then begin
    AddLinePointEx(StepEndX,StepEndY,StepEndA,StepEndB,StepEndC,StepEndD,StepEndE,StepEndF);
   end;    
   FlushStepLine;
   setlength(LinePointsBuf,0);
   NumLinePoints:=0;
  end else begin
   ConvertLineStorkeToPolygon;
   NumLinePoints:=0;
  end;
 end;
 FlushLineOnWork:=false;
end;

procedure TBeRoVectorCanvas.CloseEx;
begin
 if NeedToClose then begin
  NeedToClose:=false;
  if (mx<>lx) or (my<>ly) then begin
   RenderLine(mx,my,ma,mb,mc,md,me,mf);
   lx:=mx;
   ly:=my;
   LastX:=mX;
   LastY:=mY;
   lasta:=ma;
   lastb:=mb;
   lastc:=mc;
   lastd:=md;
   laste:=me;
   lastf:=mf;
  end;
  RecordCell;
 end;
end;

procedure TBeRoVectorCanvas.ApplyMatrix(var x,y:integer);
var tx:integer;
begin
 tx:=x;
 x:=softtrunc((tx*Matrix[0])+(y*Matrix[2])+Matrix[4]);
 y:=softtrunc((tx*Matrix[1])+(y*Matrix[3])+Matrix[5]);
end;

procedure TBeRoVectorCanvas.MoveToEx(tox,toy,toa,tob,toc,tod,toe,tof:integer);
begin
 CloseEx;
 if SaveCommands and (CurrentShape<>DefaultShape) then begin
  if CurrentShape.NumCommands>=length(CurrentShape.Commands) then begin
   setlength(CurrentShape.Commands,((CurrentShape.NumCommands+1)+memoryinc) and not memoryincmask);
  end;
  CurrentShape.Commands[CurrentShape.NumCommands].CommandType:=bvcctMOVETO;
  CurrentShape.Commands[CurrentShape.NumCommands].x:=tox;
  CurrentShape.Commands[CurrentShape.NumCommands].y:=toy;
  CurrentShape.Commands[CurrentShape.NumCommands].a:=toa;
  CurrentShape.Commands[CurrentShape.NumCommands].b:=tob;
  CurrentShape.Commands[CurrentShape.NumCommands].c:=toc;
  CurrentShape.Commands[CurrentShape.NumCommands].d:=tod;
  CurrentShape.Commands[CurrentShape.NumCommands].e:=toe;
  CurrentShape.Commands[CurrentShape.NumCommands].f:=tof;
  inc(CurrentShape.NumCommands);
 end;
 ApplyMatrix(tox,toy);
 if CurrentRenderingMode in [bvcrmRGB,bvcrmBGR] then begin
  tox:=tox*3;
  toy:=toy;
 end;
 StartCell(tox div pixelfactor,toy div pixelfactor);
 lastx:=tox;
 lasty:=toy;
 lasta:=toa;
 lastb:=tob;
 lastc:=toc;
 lastd:=tod;
 laste:=toe;
 lastf:=tof;
 ma:=toa;
 mb:=tob;
 mc:=toc;
 md:=tod;
 me:=toe;
 mf:=tof;
 mx:=tox;
 my:=toy;
 lx:=tox;
 ly:=toy;
end;

procedure TBeRoVectorCanvas.LineToEx(tox,toy,toa,tob,toc,tod,toe,tof:integer);
begin
 if SaveCommands or (CurrentShape<>DefaultShape) then begin
  if CurrentShape.NumCommands>=length(CurrentShape.Commands) then begin
   setlength(CurrentShape.Commands,((CurrentShape.NumCommands+1)+memoryinc) and not memoryincmask);
  end;
  CurrentShape.Commands[CurrentShape.NumCommands].CommandType:=bvcctLINETO;
  CurrentShape.Commands[CurrentShape.NumCommands].x:=tox;
  CurrentShape.Commands[CurrentShape.NumCommands].y:=toy;
  CurrentShape.Commands[CurrentShape.NumCommands].a:=toa;
  CurrentShape.Commands[CurrentShape.NumCommands].b:=tob;
  CurrentShape.Commands[CurrentShape.NumCommands].c:=toc;
  CurrentShape.Commands[CurrentShape.NumCommands].d:=tod;
  CurrentShape.Commands[CurrentShape.NumCommands].e:=toe;
  CurrentShape.Commands[CurrentShape.NumCommands].f:=tof;
  inc(CurrentShape.NumCommands);
 end;
 ApplyMatrix(tox,toy);
 if CurrentRenderingMode in [bvcrmRGB,bvcrmBGR] then begin
  tox:=tox*3;
  toy:=toy;
 end;
 RenderLine(tox,toy,toa,tob,toc,tod,toe,tof);
 lx:=tox;
 ly:=toy;
end;

procedure TBeRoVectorCanvas.AddLinePoint(x,y,a,b,c,d,e,f:integer);
begin
 if NumLinePoints>=length(LinePoints) then begin
  setlength(LinePoints,((NumLinePoints+1)+memoryinc) and not memoryincmask);
 end;
 LinePoints[NumLinePoints].x:=x;
 LinePoints[NumLinePoints].y:=y;
 LinePoints[NumLinePoints].d:=0;
 LinePoints[NumLinePoints].UDDa:=a;
 LinePoints[NumLinePoints].UDDb:=b;
 LinePoints[NumLinePoints].UDDc:=c;
 LinePoints[NumLinePoints].UDDd:=d;
 LinePoints[NumLinePoints].UDDe:=e;
 LinePoints[NumLinePoints].UDDf:=f;
 inc(NumLinePoints);
end;

procedure TBeRoVectorCanvas.ConvertLineStorkeToPolygon;
var i,x1,y1,x2,y2,dx,dy,d,fx,fy,fa,fb,fc,fd,fe,ff,lx,ly:integer;
    first,Closed:boolean;
    lhw:single;
 procedure Point(x,y,a,b,c,d,e,f:integer);
 begin
  if first then begin
   First:=false;
   MoveToEx(x,y,a,b,c,d,e,f);
   fx:=x;
   fy:=y;
   fa:=a;
   fb:=b;
   fc:=c;
   fd:=d;
   fe:=e;
   ff:=f;
  end else begin
   LineToEx(x,y,a,b,c,d,e,f);
  end;
  lx:=x;
  ly:=y;
 end;
 procedure CloseLine;
 begin
  if ((lx<>fx) or (ly<>fy)) and not First then begin
   Point(fx,fy,fa,fb,fc,fd,fe,ff);
  end;
 end;
 function calcintersection(ax,ay,bx,by,cx,cy,dx,dy:single;out x,y:single):boolean;
 var r,num,den:single;
 begin
  num:=((ay-cy)*(dx-cx))-((ax-cx)*(dy-cy));
  den:=((bx-ax)*(dy-cy))-((by-ay)*(dx-cx));
  if abs(den)<1.0E-14 then begin
   result:=false;
  end else begin
   r:=num/den;
   x:=ax+(r*(bx-ax));
   y:=ay+(r*(by-ay));
   result:=true;
  end;
 end;
 procedure DoArc(x,y,dx1,dy1,dx2,dy2,ua1,ua2,ub1,ub2,uc1,uc2,ud1,ud2,ue1,ue2,uf1,uf2:integer);
 var a1,a2,da:single;
     ccw:boolean;
 begin
  a1:=arctan2(dy1,dx1);
  a2:=arctan2(dy2,dx2);
  da:=a1-a2;
  ccw:=(da>0) and (da<pi);
  da:=arccos(lhw/(lhw+32))*2;
  Point(x+dx1,y+dy1,ua1,ub1,uc1,ud1,ue1,uf1);
  if ccw then begin
   if a1<a2 then begin
    a2:=a2-(2*pi);
   end;
   a2:=a2+(da*0.25);
   a1:=a1-da;
   while a1>a2 do begin
    Point(softtrunc(x+(cos(a1)*lhw)),softtrunc(y+(sin(a1)*lhw)),interpolateUDD(softtrunc(x+(cos(a1)*lhw)),ua1,ua2,x+dx1,x+dx2),interpolateUDD(softtrunc(y+(sin(a1)*lhw)),ub1,ub2,y+dy1,y+dy2),interpolateUDD(softtrunc(y+(sin(a1)*lhw)),uc1,uc2,y+dy1,y+dy2),interpolateUDD(softtrunc(y+(sin(a1)*lhw)),ud1,ud2,y+dy1,y+dy2),interpolateUDD(softtrunc(y+(sin(a1)*lhw)),ue1,ue2,y+dy1,y+dy2),interpolateUDD(softtrunc(y+(sin(a1)*lhw)),uf1,uf2,y+dy1,y+dy2));
    a1:=a1-da;
   end;
  end else begin
   if a1>a2 then begin
    a2:=a2+(2*pi);
   end;
   a2:=a2-(da*0.25);
   a1:=a1+da;
   while a1<a2 do begin
    Point(softtrunc(x+(cos(a1)*lhw)),softtrunc(y+(sin(a1)*lhw)),interpolateUDD(softtrunc(x+(cos(a1)*lhw)),ua1,ua2,x+dx1,x+dx2),interpolateUDD(softtrunc(y+(sin(a1)*lhw)),ub1,ub2,y+dy1,y+dy2),interpolateUDD(softtrunc(y+(sin(a1)*lhw)),uc1,uc2,y+dy1,y+dy2),interpolateUDD(softtrunc(y+(sin(a1)*lhw)),ud1,ud2,y+dy1,y+dy2),interpolateUDD(softtrunc(y+(sin(a1)*lhw)),ue1,ue2,y+dy1,y+dy2),interpolateUDD(softtrunc(y+(sin(a1)*lhw)),uf1,uf2,y+dy1,y+dy2));
    a1:=a1+da;
   end;
  end;
  Point(x+dx2,y+dy2,ua2,ub2,uc2,ud2,ue2,uf2);
 end;
 procedure DoMiter(p1x,p1y,p2x,p2y,p3x,p3y,dx1,dy1,dx2,dy2,ua1,ua2,ua3,ub1,ub2,ub3,uc1,uc2,uc3,ud1,ud2,ud3,ue1,ue2,ue3,uf1,uf2,uf3:integer;CurrentLineJoinMode:TBeRoVectorCanvasLineJoinMode;miterlimit:integer);
 var xi,yi,d1,lim,x2,y2:single;
     miterlimitexceeded:boolean;
 begin
  xi:=p2x;
  yi:=p2y;
  miterlimitexceeded:=true;
  if calcintersection(p1x+dx1,p1y-dy1,p2x+dx1,p2y-dy1,p2x+dx2,p2y-dy2,p3x+dx2,p3y-dy2,xi,yi) then begin
   d1:=sqrt(sqr(p2x-xi)+sqr(p2y-yi));
   lim:=(CurrentLineWidth*miterlimit) div (256*2);
   if d1<=lim then begin
    Point(softtrunc(xi),softtrunc(yi),interpolateUDD(softtrunc(xi),ua1,ua3,p1x,p3x),interpolateUDD(softtrunc(yi),ub1,ub3,p1y,p3y),interpolateUDD(softtrunc(yi),uc1,uc3,p1y,p3y),interpolateUDD(softtrunc(yi),ud1,ud3,p1y,p3y),interpolateUDD(softtrunc(yi),ue1,ue3,p1y,p3y),interpolateUDD(softtrunc(yi),uf1,uf3,p1y,p3y));
    miterlimitexceeded:=false;
   end;
  end else begin
   x2:=p2x+dx1;
   y2:=p2y-dy1;
   if ((((x2-p1x)*dy1)-((p1y-y2)*dx1))<0)<>((((x2-p3x)*dy1)-((p3y-y2)*dx1)<0)) then begin
    Point(p2x+dx1,p2y-dy1,interpolateUDD(p2x+dx1,ua1,ua3,p1x,p3x),interpolateUDD(p2y-dy1,ub1,ub3,p1y,p3y),interpolateUDD(p2y-dy1,uc1,uc3,p1y,p3y),interpolateUDD(p2y-dy1,ud1,ud3,p1y,p3y),interpolateUDD(p2y-dy1,ue1,ue3,p1y,p3y),interpolateUDD(p2y-dy1,uf1,uf3,p1y,p3y));
    miterlimitexceeded:=false;
   end;
  end;
  if miterlimitexceeded then begin
   case CurrentLineJoinMode of
    bvcljmMITERREVERT:begin
     Point(p2x+dx1,p2y-dy1,interpolateUDD(p2x+dx1,ua1,ua3,p1x,p3x),interpolateUDD(p2y-dy1,ub1,ub3,p1y,p3y),interpolateUDD(p2y-dy1,uc1,uc3,p1y,p3y),interpolateUDD(p2y-dy1,ud1,ud3,p1y,p3y),interpolateUDD(p2y-dy1,ue1,ue3,p1y,p3y),interpolateUDD(p2y-dy1,uf1,uf3,p1y,p3y));
     Point(p2x+dx2,p2y-dy2,interpolateUDD(p2x+dx2,ua1,ua3,p1x,p3x),interpolateUDD(p2y-dy2,ub1,ub3,p1y,p3y),interpolateUDD(p2y-dy2,uc1,uc3,p1y,p3y),interpolateUDD(p2y-dy2,ud1,ud3,p1y,p3y),interpolateUDD(p2y-dy2,ue1,ue3,p1y,p3y),interpolateUDD(p2y-dy2,uf1,uf3,p1y,p3y));
    end;
    bvcljmMITERROUND:begin
     DoArc(p2x,p2y,dx1,-dy1,dx2,-dy2,ua1,ua3,ub1,ub3,uc1,uc3,ud1,ud3,ue1,ue3,uf1,uf3);
    end;
    else begin
     Point(p2x+dx1+((dy1*miterlimit) div 256),p2y-dy1+((dx1*miterlimit) div 256),interpolateUDD(p2x+dx1+((dy1*miterlimit) div 256),ua1,ua3,p1x,p3x),interpolateUDD(p2y-dy1+((dx1*miterlimit) div 256),ub1,ub3,p1y,p3y),interpolateUDD(p2y-dy1+((dx1*miterlimit) div 256),uc1,uc3,p1y,p3y),interpolateUDD(p2y-dy1+((dx1*miterlimit) div 256),ud1,ud3,p1y,p3y),interpolateUDD(p2y-dy1+((dx1*miterlimit) div 256),ue1,ue3,p1y,p3y),interpolateUDD(p2y-dy1+((dx1*miterlimit) div 256),uf1,uf3,p1y,p3y));
     Point(p2x+dx2-((dy2*miterlimit) div 256),p2y-dy2-((dx2*miterlimit) div 256),interpolateUDD(p2x+dx2-((dy2*miterlimit) div 256),ua1,ua3,p1x,p3x),interpolateUDD(p2y-dy2-((dx2*miterlimit) div 256),ub1,ub3,p1y,p3y),interpolateUDD(p2y-dy2-((dx2*miterlimit) div 256),uc1,uc3,p1y,p3y),interpolateUDD(p2y-dy2-((dx2*miterlimit) div 256),ud1,ud3,p1y,p3y),interpolateUDD(p2y-dy2-((dx2*miterlimit) div 256),ue1,ue3,p1y,p3y),interpolateUDD(p2y-dy2-((dx2*miterlimit) div 256),uf1,uf3,p1y,p3y));
    end;
   end;
  end;
 end;
 procedure Join(i1,i2,i3,di1,di2:integer);
 var x1,y1,x2,y2,x3,y3,ua1,ua2,ua3,ub1,ub2,ub3,uc1,uc2,uc3,ud1,ud2,ud3,ue1,ue2,ue3,uf1,uf2,uf3,dx1,dy1,dx2,dy2,d1,d2,d:integer;
 begin
  x1:=LinePoints[i1].x;
  y1:=LinePoints[i1].y;
  x2:=LinePoints[i2].x;
  y2:=LinePoints[i2].y;
  x3:=LinePoints[i3].x;
  y3:=LinePoints[i3].y;
  ua1:=LinePoints[i1].UDDa;
  ub1:=LinePoints[i1].UDDb;
  uc1:=LinePoints[i1].UDDc;
  ud1:=LinePoints[i1].UDDd;
  ue1:=LinePoints[i1].UDDe;
  uf1:=LinePoints[i1].UDDf;
  ua2:=LinePoints[i2].UDDa;
  ub2:=LinePoints[i2].UDDb;
  uc2:=LinePoints[i2].UDDc;
  ud2:=LinePoints[i2].UDDd;
  ue2:=LinePoints[i2].UDDe;
  uf2:=LinePoints[i2].UDDf;
  ua3:=LinePoints[i3].UDDa;
  ub3:=LinePoints[i3].UDDb;
  uc3:=LinePoints[i3].UDDc;
  ud3:=LinePoints[i3].UDDd;
  ue3:=LinePoints[i3].UDDe;
  uf3:=LinePoints[i3].UDDf;
  d1:=LinePoints[di1].d;
  d2:=LinePoints[di2].d;
  dx1:=(CurrentLineWidth*(y2-y1)) div (d1*2);
  dy1:=(CurrentLineWidth*(x2-x1)) div (d1*2);
  dx2:=(CurrentLineWidth*(y3-y2)) div (d2*2);
  dy2:=(CurrentLineWidth*(x3-x2)) div (d2*2);
  if calc_point_location(x1,y1,x2,y2,x3,y3)>0 then begin
   case CurrentLineInnerJoinMode of
    bvclijmBEVEL:begin
     Point(x2+dx1,y2-dy1,interpolateUDD(x2+dx1,ua1,ua3,x1,x3),interpolateUDD(y2-dy1,ub1,ub3,y1,y3),interpolateUDD(y2-dy1,uc1,uc3,y1,y3),interpolateUDD(y2-dy1,ud1,ud3,y1,y3),interpolateUDD(y2-dy1,ue1,ue3,y1,y3),interpolateUDD(y2-dy1,uf1,uf3,y1,y3));
     Point(x2+dx2,y2-dy2,interpolateUDD(x2+dx2,ua1,ua3,x1,x3),interpolateUDD(y2-dy2,ub1,ub3,y1,y3),interpolateUDD(y2-dy2,uc1,uc3,y1,y3),interpolateUDD(y2-dy2,ud1,ud3,y1,y3),interpolateUDD(y2-dy2,ue1,ue3,y1,y3),interpolateUDD(y2-dy2,uf1,uf3,y1,y3));
    end;
    bvclijmMITER:begin
     DoMiter(x1,y1,x2,y2,x3,y3,dx1,dy1,dx2,dy2,ua1,ua2,ua3,ub1,ub2,ub3,uc1,uc2,uc3,ud1,ud2,ud3,ue1,ue2,ue3,uf1,uf2,uf3,bvcljmMITERREVERT,258);
    end;
    bvclijmJAG:begin
     d:=((dx1-dx2)*(dx1-dx2))+((dy1-dy2)*(dy1-dy2));
     if (d<(d1*d1)) and (d<(d2*d2)) then begin
      DoMiter(x1,y1,x2,y2,x3,y3,dx1,dy1,dx2,dy2,ua1,ua2,ua3,ub1,ub2,ub3,uc1,uc2,uc3,ud1,ud2,ud3,ue1,ue2,ue3,uf1,uf2,uf3,bvcljmMITERREVERT,258);
     end else begin
      Point(x2+dx1,y2-dy1,interpolateUDD(x2+dx1,ua1,ua3,x1,x3),interpolateUDD(y2-dy1,ub1,ub3,y1,y3),interpolateUDD(y2-dy1,uc1,uc3,y1,y3),interpolateUDD(y2-dy1,ud1,ud3,y1,y3),interpolateUDD(y2-dy1,ue1,ue3,y1,y3),interpolateUDD(y2-dy1,uf1,uf3,y1,y3));
      Point(x2,y2,ua2,ub2,uc2,ud2,ue2,uf2);
      Point(x2+dx2,y2-dy2,interpolateUDD(x2+dx2,ua1,ua3,x1,x3),interpolateUDD(y2-dy2,ub1,ub3,y1,y3),interpolateUDD(y2-dy2,uc1,uc3,y1,y3),interpolateUDD(y2-dy2,ud1,ud3,y1,y3),interpolateUDD(y2-dy2,ue1,ue3,y1,y3),interpolateUDD(y2-dy2,uf1,uf3,y1,y3));
     end;
    end;
    bvclijmROUND:begin
     d:=((dx1-dx2)*(dx1-dx2))+((dy1-dy2)*(dy1-dy2));
     if (d<(d1*d1)) and (d<(d2*d2)) then begin
      DoMiter(x1,y1,x2,y2,x3,y3,dx1,dy1,dx2,dy2,ua1,ua2,ua3,ub1,ub2,ub3,uc1,uc2,uc3,ud1,ud2,ud3,ue1,ue2,ue3,uf1,uf2,uf3,bvcljmMITERREVERT,258);
     end else begin
      Point(x2+dx1,y2-dy1,interpolateUDD(x2+dx1,ua1,ua3,x1,x3),interpolateUDD(y2-dy1,ub1,ub3,y1,y3),interpolateUDD(y2-dy1,uc1,uc3,y1,y3),interpolateUDD(y2-dy1,ud1,ud3,y1,y3),interpolateUDD(y2-dy1,ue1,ue3,y1,y3),interpolateUDD(y2-dy1,uf1,uf3,y1,y3));
      Point(x2,y2,ua2,ub2,uc2,ud2,ue2,uf2);
      DoArc(x2,y2,dx2,-dy2,dx1,-dy1,ua1,ua3,ub1,ub3,uc1,uc3,ud1,ud3,ue1,ue3,uf1,uf3);
      Point(x2,y2,ua2,ub2,uc2,ud2,ue2,uf2);
      Point(x2+dx2,y2-dy2,interpolateUDD(x2+dx2,ua1,ua3,x1,x3),interpolateUDD(y2-dy2,ub1,ub3,y1,y3),interpolateUDD(y2-dy2,uc1,uc3,y1,y3),interpolateUDD(y2-dy2,ud1,ud3,y1,y3),interpolateUDD(y2-dy2,ue1,ue3,y1,y3),interpolateUDD(y2-dy2,uf1,uf3,y1,y3));
     end;
    end;
   end;
  end else begin
   case CurrentLineJoinMode of
    bvcljmBEVEL:begin
     Point(x2+dx1,y2-dy1,interpolateUDD(x2+dx1,ua1,ua3,x1,x3),interpolateUDD(y2-dy1,ub1,ub3,y1,y3),interpolateUDD(y2-dy1,uc1,uc3,y1,y3),interpolateUDD(y2-dy1,ud1,ud3,y1,y3),interpolateUDD(y2-dy1,ue1,ue3,y1,y3),interpolateUDD(y2-dy1,uf1,uf3,y1,y3));
     Point(x2+dx2,y2-dy2,interpolateUDD(x2+dx2,ua1,ua3,x1,x3),interpolateUDD(y2-dy2,ub1,ub3,y1,y3),interpolateUDD(y2-dy2,uc1,uc3,y1,y3),interpolateUDD(y2-dy2,ud1,ud3,y1,y3),interpolateUDD(y2-dy2,ue1,ue3,y1,y3),interpolateUDD(y2-dy2,uf1,uf3,y1,y3));
    end;
    bvcljmROUND:begin
     DoArc(x2,y2,dx1,-dy1,dx2,-dy2,ua1,ua3,ub1,ub3,uc1,uc3,ud1,ud3,ue1,ue3,uf1,uf3);
    end;
    bvcljmMITER,bvcljmMITERREVERT,bvcljmMITERROUND:begin
     DoMiter(x1,y1,x2,y2,x3,y3,dx1,dy1,dx2,dy2,ua1,ua2,ua3,ub1,ub2,ub3,uc1,uc2,uc3,ud1,ud2,ud3,ue1,ue2,ue3,uf1,uf2,uf3,CurrentLineJoinMode,1024);
    end;
   end;
  end;
 end;
 procedure Cap(i1,i2,di:integer);
 var x1,y1,x2,y2,ua1,ua2,ub1,ub2,uc1,uc2,ud1,ud2,ue1,ue2,uf1,uf2,dx1,dy1,dx2,dy2,d:integer;
     a1,a2,da:single;
 begin
  x1:=LinePoints[i1].x;
  y1:=LinePoints[i1].y;
  x2:=LinePoints[i2].x;
  y2:=LinePoints[i2].y;
  ua1:=LinePoints[i1].UDDa;
  ub1:=LinePoints[i1].UDDb;
  uc1:=LinePoints[i1].UDDc;
  ud1:=LinePoints[i1].UDDd;
  ue1:=LinePoints[i1].UDDe;
  uf1:=LinePoints[i1].UDDf;
  ua2:=LinePoints[i2].UDDa;
  ub2:=LinePoints[i2].UDDb;
  uc2:=LinePoints[i2].UDDc;
  ud2:=LinePoints[i2].UDDd;
  ue2:=LinePoints[i2].UDDe;
  uf2:=LinePoints[i2].UDDf;
  d:=LinePoints[di].d;
  dx1:=(CurrentLineWidth*(y2-y1)) div (2*d);
  dy1:=(CurrentLineWidth*(x2-x1)) div (2*d);
  case CurrentLineCapMode of
   bvclcmROUND:begin
    Point(x1-dx1,y1+dy1,interpolateUDD(x1-dx1,ua1,ua2,x1,x2),interpolateUDD(y1+dy1,ub1,ub2,y1,y2),interpolateUDD(y1+dy1,uc1,uc2,y1,y2),interpolateUDD(y1+dy1,ud1,ud2,y1,y2),interpolateUDD(y1+dy1,ue1,ue2,y1,y2),interpolateUDD(y1+dy1,uf1,uf2,y1,y2));
    if dx1=0 then begin
     dx1:=1;
    end;
    a1:=arctan2(dy1,-dx1);
    a2:=a1+pi;
    da:=arccos(lhw/(lhw+32))*2;
    a1:=a1+da;
    a2:=a2-(da*0.25);
    while a1<a2 do begin
     Point(softtrunc(x1+(cos(a1)*lhw)),softtrunc(y1+(sin(a1)*lhw)),interpolateUDD(softtrunc(x1+(cos(a1)*lhw)),ua1,ua2,x1,x2),interpolateUDD(softtrunc(y1+(sin(a1)*lhw)),ub1,ub2,y1,y2),interpolateUDD(softtrunc(y1+(sin(a1)*lhw)),uc1,uc2,y1,y2),interpolateUDD(softtrunc(y1+(sin(a1)*lhw)),ud1,ud2,y1,y2),interpolateUDD(softtrunc(y1+(sin(a1)*lhw)),ue1,ue2,y1,y2),interpolateUDD(softtrunc(y1+(sin(a1)*lhw)),uf1,uf2,y1,y2));
     a1:=a1+da;
    end;
    Point(x1+dx1,y1-dy1,interpolateUDD(x1+dx1,ua1,ua2,x1,x2),interpolateUDD(y1-dy1,ub1,ub2,y1,y2),interpolateUDD(y1-dy1,uc1,uc2,y1,y2),interpolateUDD(y1-dy1,ud1,ud2,y1,y2),interpolateUDD(y1-dy1,ue1,ue2,y1,y2),interpolateUDD(y1-dy1,uf1,uf2,y1,y2));
   end;
   else begin
    case CurrentLineCapMode of
     bvclcmSQUARE:begin
      dx2:=dx1;
      dy2:=dy1;
     end;
     else begin
      dx2:=0;
      dy2:=0;
     end;
    end;
    Point(x1-dx1-dx2,y1+dy1-dy2,interpolateUDD(x1-dx1-dx2,ua1,ua2,x1,x2),interpolateUDD(y1+dy1-dy2,ub1,ub2,y1,y2),interpolateUDD(y1+dy1-dy2,uc1,uc2,y1,y2),interpolateUDD(y1+dy1-dy2,ud1,ud2,y1,y2),interpolateUDD(y1+dy1-dy2,ue1,ue2,y1,y2),interpolateUDD(y1+dy1-dy2,uf1,uf2,y1,y2));
    Point(x1+dx1-dx2,y1-dy1-dy2,interpolateUDD(x1+dx1-dx2,ua1,ua2,x1,x2),interpolateUDD(y1-dy1-dy2,ub1,ub2,y1,y2),interpolateUDD(y1-dy1-dy2,uc1,uc2,y1,y2),interpolateUDD(y1-dy1-dy2,ud1,ud2,y1,y2),interpolateUDD(y1-dy1-dy2,ue1,ue2,y1,y2),interpolateUDD(y1-dy1-dy2,uf1,uf2,y1,y2));
   end;
  end;
 end;
begin
 if NumLinePoints<2 then exit;
 Closed:=false;
 if (LinePoints[0].x=LinePoints[NumLinePoints-1].x) and (LinePoints[0].y=LinePoints[NumLinePoints-1].y) then begin
  dec(NumLinePoints);
  Closed:=true;
 end;
 for i:=0 to NumLinePoints-1 do begin
  x1:=LinePoints[i].x;
  y1:=LinePoints[i].y;
  x2:=LinePoints[(i+1) mod NumLinePoints].x;
  y2:=LinePoints[(i+1) mod NumLinePoints].y;
  dx:=x2-x1;
  dy:=y2-y1;
  d:=VectorLength(dx,dy);
  if d=0 then begin
   d:=1;
  end;
  LinePoints[i].d:=d;
 end;
 lhw:=abs(CurrentLineWidth*0.5);
 if Closed then begin
  First:=true;
  for i:=0 to NumLinePoints-1 do begin
   Join((i+NumLinePoints-1) mod NumLinePoints,i,(i+1) mod NumLinePoints,(i+NumLinePoints-1) mod NumLinePoints,i);
  end;
  CloseLine;
  First:=true;
  for i:=NumLinePoints-1 downto 0 do begin
   Join((i+1) mod NumLinePoints,i,(i+NumLinePoints-1) mod NumLinePoints,i,(i+NumLinePoints-1) mod NumLinePoints);
  end;
  CloseLine;
 end else begin
  First:=true;
  Cap(0,1,0);
  for i:=1 to NumLinePoints-2 do begin
   Join((i+NumLinePoints-1) mod NumLinePoints,i,(i+1) mod NumLinePoints,(i+NumLinePoints-1) mod NumLinePoints,i);
  end;
  Cap(NumLinePoints-1,NumLinePoints-2,NumLinePoints-2);
  for i:=NumLinePoints-2 downto 1 do begin
   Join((i+1) mod NumLinePoints,i,(i+NumLinePoints-1) mod NumLinePoints,i,(i+NumLinePoints-1) mod NumLinePoints);
  end;
 end;
 CloseLine;
end;

procedure TBeRoVectorCanvas.SetWidth(NewWidth:integer);
begin
 Setup(NewWidth,CurrentHeight,CurrentRenderingMode);
end;

procedure TBeRoVectorCanvas.SetHeight(NewHeight:integer);
begin
 Setup(CurrentWidth,NewHeight,CurrentRenderingMode);
end;

procedure TBeRoVectorCanvas.SetRenderingMode(NewRenderingMode:TBeRoVectorCanvasRenderingMode);
begin
 Setup(CurrentWidth,CurrentHeight,NewRenderingMode);
end;

procedure TBeRoVectorCanvas.SetColor(AColor:longword);
begin
 CurrentColor:=AColor;
end;

procedure TBeRoVectorCanvas.SetWinding(AWinding:boolean);
begin
 CurrentWinding:=AWinding;
end;

procedure TBeRoVectorCanvas.SetStyleMode(AStyleMode:TBeRoVectorCanvasStyleMode);
begin
 CurrentStyleMode:=AStyleMode;
end;

procedure TBeRoVectorCanvas.SetLineWidth(ALineWidth:integer);
begin
 CurrentLineWidth:=ALineWidth;
 if CurrentLineWidth=0 then begin
  CurrentLineWidth:=1;
 end;
end;

procedure TBeRoVectorCanvas.SetLineCapMode(ALineCapMode:TBeRoVectorCanvasLineCapMode);
begin
 CurrentLineCapMode:=ALineCapMode;
end;

procedure TBeRoVectorCanvas.SetLineJoinMode(ALineJoinMode:TBeRoVectorCanvasLineJoinMode);
begin
 CurrentLineJoinMode:=ALineJoinMode;
end;

procedure TBeRoVectorCanvas.SetLineInnerJoinMode(ALineInnerJoinMode:TBeRoVectorCanvasLineInnerJoinMode);
begin
 CurrentLineInnerJoinMode:=ALineInnerJoinMode;
end;

procedure TBeRoVectorCanvas.SetGamma(AGamma:single);
const div255=1/255;
var i,j:integer;
begin
 if CurrentGamma<>AGamma then begin
  CurrentGamma:=AGamma;
  for i:=low(TBeRoVectorCanvasGammaLookUpTable) to high(TBeRoVectorCanvasGammaLookUpTable) do begin
   if (i=0) or (AGamma<1) then begin
    j:=0;
   end else begin
    j:=softtrunc(pow(i*div255,AGamma)*255);
    if j<0 then begin
     j:=0;
    end else if j>255 then begin
     j:=255;
    end;
   end;
   GammaLookUpTable[i]:=j;
  end;
 end;
end;

procedure TBeRoVectorCanvas.SetHandleUDD(AHandleUDD:boolean);
begin
 CurrentHandleUDD:=AHandleUDD;
end;

procedure TBeRoVectorCanvas.MoveTo(tox,toy:integer;toa:integer=0;tob:integer=0;toc:integer=0;tod:integer=0;toe:integer=0;tof:integer=0);
begin
 FlushLine;
 LastCX:=toX;
 LastCY:=toY;
 case CurrentStyleMode of
  bvcsmFILL:begin
   MoveToEx(tox,toy,toa,tob,toc,tod,toe,tof);
  end;
  bvcsmLINE:begin
   AddLinePoint(tox,toy,toa,tob,toc,tod,toe,tof);
  end;
 end;
end;

procedure TBeRoVectorCanvas.LineTo(tox,toy:integer;toa:integer=0;tob:integer=0;toc:integer=0;tod:integer=0;toe:integer=0;tof:integer=0);
begin
 LastCX:=toX;
 LastCY:=toY;
 case CurrentStyleMode of
  bvcsmFILL:begin
   LineToEx(tox,toy,toa,tob,toc,tod,toe,tof);
  end;
  bvcsmLINE:begin
   AddLinePoint(tox,toy,toa,tob,toc,tod,toe,tof);
  end;
 end;
end;

procedure TBeRoVectorCanvas.QuadraticCurveTo(cx,cy,ax,ay:integer;ca:integer=0;cb:integer=0;cc:integer=0;cd:integer=0;ce:integer=0;cf:integer=0;aa:integer=0;ab:integer=0;ac:integer=0;ad:integer=0;ae:integer=0;af:integer=0;tolerance:integer=2;maxlevel:integer=32);
 procedure Recursive(x1,y1,x2,y2,x3,y3,a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2,a3,b3,c3,d3,e3,f3,level:integer);
 var x12,y12,a12,b12,c12,d12,e12,f12,x23,y23,a23,b23,c23,d23,e23,f23,x123,y123,a123,b123,c123,d123,e123,f123,mx,my,d:integer;
 begin
  x12:=(x1+x2) div 2;
  y12:=(y1+y2) div 2;
  a12:=(a1+a2) div 2;
  b12:=(b1+b2) div 2;
  c12:=(c1+c2) div 2;
  d12:=(d1+d2) div 2;
  e12:=(e1+e2) div 2;
  f12:=(f1+f2) div 2;
  x23:=(x2+x3) div 2;
  y23:=(y2+y3) div 2;
  a23:=(a2+a3) div 2;
  b23:=(b2+b3) div 2;
  c23:=(c2+c3) div 2;
  d23:=(d2+d3) div 2;
  e23:=(e2+e3) div 2;
  f23:=(f2+f3) div 2;
  x123:=(x12+x23) div 2;
  y123:=(y12+y23) div 2;
  a123:=(a12+a23) div 2;
  b123:=(b12+b23) div 2;
  c123:=(c12+c23) div 2;
  d123:=(d12+d23) div 2;
  e123:=(e12+e23) div 2;
  f123:=(f12+f23) div 2;
  mx:=(x1+x3) div 2;
  my:=(y1+y3) div 2;                             
  d:=abs(mx-x123)+abs(my-y123);
  if (level>maxlevel) or (d<tolerance) then begin
   LineTo(x123,y123,a123,b123,c123,d123,e123,f123);
  end else begin
   Recursive(x1,y1,x12,y12,x123,y123,a1,b1,c1,d1,e1,f1,a12,b12,c12,d12,e12,f12,a123,b123,c123,d123,e123,f123,level+1);
   Recursive(x123,y123,x23,y23,x3,y3,a123,b123,c123,d123,e123,f123,a23,b23,c23,d23,e23,f23,a3,b3,c3,d3,e3,f3,level+1);
  end;
 end;
begin
 Recursive(lastcx,lastcy,cx,cy,ax,ay,lasta,lastb,lastc,lastd,laste,lastf,ca,cb,cc,cd,ce,cf,aa,ab,ac,ad,ae,af,0);
 LineTo(ax,ay,aa,ab,ac,ad,ae,af);
end;

procedure TBeRoVectorCanvas.CubicCurveTo(c1x,c1y,c2x,c2y,ax,ay:integer;c1a:integer=0;c1b:integer=0;c1c:integer=0;c1d:integer=0;c1e:integer=0;c1f:integer=0;c2a:integer=0;c2b:integer=0;c2c:integer=0;c2d:integer=0;c2e:integer=0;c2f:integer=0;aa:integer=0;ab:integer=0;ac:integer=0;ad:integer=0;ae:integer=0;af:integer=0;tolerance:integer=2;maxlevel:integer=32);
 procedure Recursive(x1,y1,x2,y2,x3,y3,x4,y4,a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2,a3,b3,c3,d3,e3,f3,a4,b4,c4,d4,e4,f4,level:integer);
 var x12,y12,a12,b12,c12,d12,e12,f12,x23,y23,a23,b23,c23,d23,e23,f23,x34,y34,a34,b34,c34,d34,e34,f34,x123,y123,a123,b123,c123,d123,e123,f123,x234,y234,a234,b234,c234,d234,e234,f234,x1234,y1234,a1234,b1234,c1234,d1234,e1234,f1234,mx,my,d:integer;
 begin
  x12:=(x1+x2) div 2;
  y12:=(y1+y2) div 2;
  a12:=(a1+a2) div 2;
  b12:=(b1+b2) div 2;
  c12:=(c1+c2) div 2;
  d12:=(d1+d2) div 2;
  e12:=(e1+e2) div 2;
  f12:=(f1+f2) div 2;
  x23:=(x2+x3) div 2;
  y23:=(y2+y3) div 2;
  a23:=(a2+a3) div 2;
  b23:=(b2+b3) div 2;
  c23:=(c2+c3) div 2;
  d23:=(d2+d3) div 2;
  e23:=(e2+e3) div 2;
  f23:=(f2+f3) div 2;
  x34:=(x3+x4) div 2;
  y34:=(y3+y4) div 2;
  a34:=(a3+a4) div 2;
  b34:=(b3+b4) div 2;
  c34:=(c3+c4) div 2;
  d34:=(d3+d4) div 2;
  e34:=(e3+e4) div 2;
  f34:=(f3+f4) div 2;
  x123:=(x12+x23) div 2;
  y123:=(y12+y23) div 2;
  a123:=(a12+a23) div 2;
  b123:=(b12+b23) div 2;
  c123:=(c12+c23) div 2;
  d123:=(d12+d23) div 2;
  e123:=(e12+e23) div 2;
  f123:=(f12+f23) div 2;
  x234:=(x23+x34) div 2;
  y234:=(y23+y34) div 2;
  a234:=(a23+a34) div 2;
  b234:=(b23+b34) div 2;
  c234:=(c23+c34) div 2;
  d234:=(d23+d34) div 2;
  e234:=(e23+e34) div 2;
  f234:=(f23+f34) div 2;
  x1234:=(x123+x234) div 2;
  y1234:=(y123+y234) div 2;
  a1234:=(a123+a234) div 2;
  b1234:=(b123+b234) div 2;
  c1234:=(c123+c234) div 2;
  d1234:=(d123+d234) div 2;
  e1234:=(e123+e234) div 2;
  f1234:=(f123+f234) div 2;
  mx:=(x1+x4) div 2;
  my:=(y1+y4) div 2;
  d:=abs(mx-x1234)+abs(my-y1234);
  if (level>maxlevel) or (d<tolerance) then begin
   LineTo(x1234,y1234,a1234,b1234,c1234,d1234,e1234,f1234);
  end else begin
   Recursive(x1,y1,x12,y12,x123,y123,x1234,y1234,a1,b1,c1,d1,e1,f1,a12,b12,c12,d12,e12,f12,a123,b123,c123,d123,e123,f123,a1234,b1234,c1234,d1234,e1234,f1234,level+1);
   Recursive(x1234,y1234,x234,y234,x34,y34,x4,y4,a1234,b1234,c1234,d1234,e1234,f1234,a234,b234,c234,d234,e234,f234,a34,b34,c34,d34,e34,f34,a4,b4,c4,d4,e4,f4,level+1);
  end;
 end;
begin
 Recursive(lastcx,lastcy,c1x,c1y,c2x,c2y,ax,ay,lasta,lastb,lastc,lastd,laste,lastf,c1a,c1b,c1c,c1d,c1e,c1f,c2a,c2b,c2c,c2d,c2e,c2f,aa,ab,ac,ad,ae,af,0);
 LineTo(ax,ay,aa,ab,ac,ad,ae,af);
end;

procedure TBeRoVectorCanvas.Ellipse(x,y,rx,ry:integer;steps:integer=180;a1:integer=0;b1:integer=0;c1:integer=0;d1:integer=0;e1:integer=0;f1:integer=0;a2:integer=0;b2:integer=0;c2:integer=0;d2:integer=0;e2:integer=0;f2:integer=0);
var i,px,py,a,b,c,d,e,f:integer;
    ar,br,cr,dr,er,fr,am,bm,cm,dm,em,fm,j:single;
    pisteps:single;
begin
 pisteps:=pi/(steps*0.5);
 ar:=(a2-a1)*0.5;
 br:=(b2-b1)*0.5;
 cr:=(c2-c1)*0.5;
 dr:=(d2-d1)*0.5;
 er:=(e2-e1)*0.5;
 fr:=(f2-f1)*0.5;
 am:=(a1+a2)*0.5;
 bm:=(b1+b2)*0.5;
 cm:=(c1+c2)*0.5;
 dm:=(d1+d2)*0.5;
 em:=(e1+e2)*0.5;
 fm:=(f1+f2)*0.5;
 if HandleUDD then begin
  for i:=0 to steps do begin
   j:=i*pisteps;
   px:=softtrunc(x+(cos(j)*rx));
   py:=softtrunc(y+(sin(j)*ry));
   a:=softtrunc(am+(cos(j)*ar));
   b:=softtrunc(bm+(sin(j)*br));
   c:=softtrunc(cm+(cos(j)*cr));
   d:=softtrunc(dm+(sin(j)*dr));
   e:=softtrunc(em+(cos(j)*er));
   f:=softtrunc(fm+(sin(j)*fr));
   if i=0 then begin
    MoveTo(px,py,a,b,c,d,e,f);
   end else begin
    LineTo(px,py,a,b,c,d,e,f);
   end;
  end;
 end else begin
  for i:=0 to steps do begin
   j:=i*pisteps;
   px:=softtrunc(x+(cos(j)*rx));
   py:=softtrunc(y+(sin(j)*ry));
   if i=0 then begin
    MoveTo(px,py);
   end else begin
    LineTo(px,py);
   end;
  end;
 end;
end;

procedure TBeRoVectorCanvas.Rectangle(x1,y1,x2,y2:integer;a1:integer=0;b1:integer=0;c1:integer=0;d1:integer=0;e1:integer=0;f1:integer=0;a2:integer=0;b2:integer=0;c2:integer=0;d2:integer=0;e2:integer=0;f2:integer=0);
begin
 MoveTo(x1,y1,a1,b1,c1,d1,e1,f1);
 LineTo(x2,y1,a2,b1,c2,d1,e2,f1);
 LineTo(x2,y2,a2,a2,c2,d2,e2,f2);
 LineTo(x1,y2,a1,a2,c1,d2,e1,f2);
 LineTo(x1,y1,a1,b1,c1,d1,e1,f1);
end;

procedure TBeRoVectorCanvas.Polygon(const Points:array of TBeRoVectorCanvasPoint);
var i:integer;
begin
 for i:=0 to length(Points)-1 do begin
  if i=0 then begin
   with Points[i] do begin
    MoveTo(x,y,a,b,c,d,e,f);
   end;
  end else begin
   with Points[i] do begin
    LineTo(x,y,a,b,c,d,e,f);
   end;
  end;
 end;
end;

procedure TBeRoVectorCanvas.PolyPolygon(const PolygonPoints:array of TBeRoVectorCanvasPoints);
var i:integer;
begin
 for i:=0 to length(PolygonPoints)-1 do begin
  Polygon(PolygonPoints[i]);
 end;
end;

procedure TBeRoVectorCanvas.Close;
begin
 FlushLine;
 CloseEx;
end;

procedure TBeRoVectorCanvas.DrawUDDBuffer;
 procedure AddUDDCell(x,y,a,b,c,d,e,f:integer);
 var i,oi,ni:integer;
     uc:PBeRoVectorCanvasUDDCell;
 begin
  if (y<0) or (y>=length(CurrentShape.ScanLines)) then exit;
  if CurrentShape.numuddcells>=length(CurrentShape.uddcells) then begin
   oi:=length(CurrentShape.uddcells);
   ni:=((CurrentShape.numuddcells+1)+memoryinc) and not memoryincmask;
   setlength(CurrentShape.uddcells,ni);
   for i:=oi to ni-1 do begin
    new(CurrentShape.uddcells[i]);
   end;
  end;
  uc:=CurrentShape.uddcells[CurrentShape.numuddcells];
  inc(CurrentShape.numuddcells);
  uc^.x:=x;
  uc^.a:=a;
  uc^.b:=b;
  uc^.c:=c;
  uc^.d:=d;
  uc^.e:=e;
  uc^.f:=f;
  if CurrentShape.ScanLines[y].numUDDcells>=length(CurrentShape.ScanLines[y].UDDcells) then begin
   setlength(CurrentShape.ScanLines[y].UDDcells,((CurrentShape.ScanLines[y].numUDDcells+1)+memoryinc) and not memoryincmask);
  end;
  if (CurrentShape.ScanLines[y].numUDDcells>1) and (CurrentShape.ScanLines[y].UDDcells[CurrentShape.ScanLines[y].numUDDcells-1]^.x=x) then begin
   dec(CurrentShape.ScanLines[y].numUDDcells);
  end;
  CurrentShape.ScanLines[y].UDDcells[CurrentShape.ScanLines[y].numUDDcells]:=uc;
  inc(CurrentShape.ScanLines[y].numUDDcells);
 end;
 procedure MakeUDDCells;
 var i,j,x1,y1,x2,y2,a1,a2,b1,b2,c1,c2,d1,d2,e1,e2,f1,f2,x,y,sy,ey,sx,ex,dx,dy,dl,a,b,c,d,e,f:integer;
 begin
  for i:=0 to CurrentShape.NumUDDLines-1 do begin
   x1:=CurrentShape.UDDLines[i].x1 div pixelfactor;
   y1:=CurrentShape.UDDLines[i].y1 div pixelfactor;
   x2:=CurrentShape.UDDLines[i].x2 div pixelfactor;
   y2:=CurrentShape.UDDLines[i].y2 div pixelfactor;
   a1:=CurrentShape.UDDLines[i].a1;
   a2:=CurrentShape.UDDLines[i].a2;
   b1:=CurrentShape.UDDLines[i].b1;
   b2:=CurrentShape.UDDLines[i].b2;
   c1:=CurrentShape.UDDLines[i].c1;
   c2:=CurrentShape.UDDLines[i].c2;
   d1:=CurrentShape.UDDLines[i].d1;
   d2:=CurrentShape.UDDLines[i].d2;
   e1:=CurrentShape.UDDLines[i].e1;
   e2:=CurrentShape.UDDLines[i].e2;
   f1:=CurrentShape.UDDLines[i].f1;
   f2:=CurrentShape.UDDLines[i].f2;
   if x1<x2 then begin
    sx:=x1;
    ex:=x2;
   end else begin
    sx:=x2;
    ex:=x1;
   end;
   if y1<y2 then begin
    sy:=y1;
    ey:=y2;
   end else begin
    sy:=y2;
    ey:=y1;
   end;
   dx:=x2-x1;
   dy:=y2-y1;
   if dy=0 then begin
    y:=y1;
    if dx=0 then begin
     x:=x1;
     AddUDDCell(x,y,a1,b1,c1,d1,e1,f1);
    end else begin
     for x:=sx to ex do begin
      a:=interpolateUDD(x,a1,a2,x1,x2);
      b:=interpolateUDD(x,b1,b2,x1,x2);
      c:=interpolateUDD(x,c1,c2,x1,x2);
      d:=interpolateUDD(x,d1,d2,x1,x2);
      e:=interpolateUDD(x,e1,f2,x1,x2);
      f:=interpolateUDD(x,f1,f2,x1,x2);
      AddUDDCell(x,y,a,b,c,d,e,f);
     end;
    end;
   end else begin
    if dx=0 then begin
     x:=x1;
     for y:=sy to ey do begin
      a:=interpolateUDD(y,a1,a2,y1,y2);
      b:=interpolateUDD(y,b1,b2,y1,y2);
      c:=interpolateUDD(y,c1,c2,y1,y2);
      d:=interpolateUDD(y,d1,d2,y1,y2);
      e:=interpolateUDD(y,e1,f2,y1,y2);
      f:=interpolateUDD(y,f1,f2,y1,y2);
      AddUDDCell(x,y,a,b,c,d,e,f);
     end;
    end else begin
     dl:=VectorLength(dx,dy)+1;
     for j:=1 to dl do begin
      x:=interpolateUDD(j,x1,x2,1,dl);
      y:=interpolateUDD(j,y1,y2,1,dl);
      a:=interpolateUDD(j,a1,a2,1,dl);
      b:=interpolateUDD(j,b1,b2,1,dl);
      c:=interpolateUDD(j,c1,c2,1,dl);
      d:=interpolateUDD(j,d1,d2,1,dl);
      e:=interpolateUDD(j,e1,e2,1,dl);
      f:=interpolateUDD(j,f1,f2,1,dl);
      AddUDDCell(x,y,a,b,c,d,e,f);
     end;
    end;
   end;
  end;
 end;
 procedure SortUDDCells;
 var i:integer;
  procedure QuickSort(var ScanLine:TBeRoVectorCanvasScanLine;l,r:integer);
  var i,j:integer;
      p,t:PBeRoVectorCanvasUDDCell;
  begin
   repeat
    i:=l;
    j:=r;
    p:=ScanLine.UDDcells[(l+r) div 2];
    repeat
     while ScanLine.UDDcells[i]^.x<p^.x do inc(i);
     while ScanLine.UDDcells[j]^.x>p^.x do dec(j);
     if i<=j then begin
      t:=ScanLine.UDDcells[i];
      ScanLine.UDDcells[i]:=ScanLine.UDDcells[j];
      ScanLine.UDDcells[j]:=t;
      inc(i);
      dec(j);
     end;
    until i>j;
    if l<j then begin
     QuickSort(ScanLine,l,j);
    end;
    l:=i;
   until i>=r;
  end;
  procedure BeRoSort(var ScanLine:TBeRoVectorCanvasScanLine);
  var i:integer;
      t:PBeRoVectorCanvasUDDCell;
  begin
   i:=0;
   while i<(ScanLine.numUDDcells-1) do begin
    if ScanLine.UDDcells[i]^.x>ScanLine.UDDcells[i+1]^.x then begin
     t:=ScanLine.UDDcells[i];
     ScanLine.UDDcells[i]:=ScanLine.UDDcells[i+1];
     ScanLine.UDDcells[i+1]:=t;
     if i>0 then begin
      dec(i);
     end else begin
      inc(i);
     end;
    end else begin
     inc(i);
    end;
   end;
  end;
 begin
  for i:=renderminy to rendermaxy do begin
   if CurrentShape.scanlines[i].numUDDcells<2 then continue;
   if CurrentShape.scanlines[i].numUDDcells>2 then begin
    QuickSort(CurrentShape.scanlines[i],0,CurrentShape.scanlines[i].numUDDcells-1);
   end;
   BeRoSort(CurrentShape.scanlines[i]);
  end;
 end;
 procedure OptimizeUDDCells;
 var i,j,k:integer;
 begin
  for i:=renderminy to rendermaxy do begin
   j:=1;
   while j<(CurrentShape.scanlines[i].numUDDcells-1) do begin
    if CurrentShape.scanlines[i].UDDcells[j]^.x=CurrentShape.scanlines[i].UDDcells[j-1]^.x then begin
     k:=CurrentShape.scanlines[i].numUDDcells-(j+1);
     if k>0 then begin
      move(CurrentShape.scanlines[i].UDDcells[j+1],CurrentShape.scanlines[i].UDDcells[j],k*sizeof(PBeRoVectorCanvasUDDCell));
     end;
     dec(CurrentShape.scanlines[i].numUDDcells);
    end else begin
     inc(j);
    end;
   end;
  end;
 end;
 procedure DrawUDDCells;
 var i,j,x,y,sx,ex,x1,x2,a1,a2,b1,b2,c1,c2,d1,d2,e1,e2,f1,f2:integer;
     p,uc1,uc2:PBeRoVectorCanvasUDDCell;
 begin
  for y:=renderminy to rendermaxy do begin
   if CurrentShape.scanlines[y].numUDDcells<2 then continue;
   for i:=0 to CurrentShape.scanlines[y].numUDDcells-2 do begin
    uc1:=CurrentShape.scanlines[y].UDDcells[i];
    uc2:=CurrentShape.scanlines[y].UDDcells[i+1];
    x1:=uc1^.x;
    a1:=uc1^.a;
    b1:=uc1^.b;
    c1:=uc1^.c;
    d1:=uc1^.d;
    e1:=uc1^.e;
    f1:=uc1^.f;
    x2:=uc2^.x;
    a2:=uc2^.a;
    b2:=uc2^.b;
    c2:=uc2^.c;
    d2:=uc2^.d;
    e2:=uc2^.e;
    f2:=uc2^.f;
    sx:=x1;
    if sx<0 then begin
     sx:=0;
    end;
    ex:=x2;
    if ex>=CurrentWidthEx then begin
     ex:=CurrentWidthEx-1;
    end;
    if (i=0) or (i=(CurrentShape.scanlines[y].numUDDcells-2)) or (y=renderminy) or (y=rendermaxy) then begin
     if RenderingMode in [bvcrmRGB,bvcrmBGR] then begin
      for j:=1 to UDDExtraPixels*3 do begin
       if (i=0) and (sx>0) then begin
        dec(sx);
       end;
       if (i=(CurrentShape.scanlines[y].numUDDcells-2)) and ((ex+1)<CurrentWidthEx) then begin
        inc(ex);
       end;
      end;
     end else begin
      for j:=1 to UDDExtraPixels do begin
       if (i=0) and (sx>0) then begin
        dec(sx);
       end;
       if (i=(CurrentShape.scanlines[y].numUDDcells-2)) and ((ex+1)<CurrentWidthEx) then begin
        inc(ex);
       end;
      end;
     end;
     for j:=1 to UDDExtraPixels do begin
      if (y=renderminy) and ((y-j)>=0) then begin
       p:=BufferCanvasUDD;
       inc(p,((y-j)*CurrentWidthEx)+sx);
       for x:=sx to ex do begin
        p^.a:=interpolateUDD(x,a1,a2,x1,x2);
        p^.b:=interpolateUDD(x,b1,b2,x1,x2);
        p^.c:=interpolateUDD(x,c1,c2,x1,x2);
        p^.d:=interpolateUDD(x,d1,d2,x1,x2);
        p^.e:=interpolateUDD(x,e1,e2,x1,x2);
        p^.f:=interpolateUDD(x,f1,f2,x1,x2);
        inc(p);
       end;
      end;
      if (y=rendermaxy) and ((y+j)<length(CurrentShape.scanlines)) then begin
       p:=BufferCanvasUDD;
       inc(p,((y+j)*CurrentWidthEx)+sx);
       for x:=sx to ex do begin
        p^.a:=interpolateUDD(x,a1,a2,x1,x2);
        p^.b:=interpolateUDD(x,b1,b2,x1,x2);
        p^.c:=interpolateUDD(x,c1,c2,x1,x2);
        p^.d:=interpolateUDD(x,d1,d2,x1,x2);
        p^.e:=interpolateUDD(x,e1,e2,x1,x2);
        p^.f:=interpolateUDD(x,f1,f2,x1,x2);
        inc(p);
       end;
      end;
     end;
    end;
    p:=BufferCanvasUDD;
    inc(p,(y*CurrentWidthEx)+sx);
    for x:=sx to ex do begin
     p^.a:=interpolateUDD(x,a1,a2,x1,x2);
     p^.b:=interpolateUDD(x,b1,b2,x1,x2);
     p^.c:=interpolateUDD(x,c1,c2,x1,x2);
     p^.d:=interpolateUDD(x,d1,d2,x1,x2);
     p^.e:=interpolateUDD(x,e1,e2,x1,x2);
     p^.f:=interpolateUDD(x,f1,f2,x1,x2);
     inc(p);
    end;
   end;
  end;
 end;
begin
 if CurrentShape.numUDDcells=0 then begin
  MakeUDDCells;
  SortUDDCells;
  OptimizeUDDCells;
 end;
 DrawUDDCells;
end;

procedure TBeRoVectorCanvas.Draw;
var p:^byte;
    pudd:PBeRoVectorCanvasUDDCell;
begin
 CheckScanLinesArray;
 CurrentInvertedMatrix:=MatrixInverse(Matrix);
 Close;
 if CurrentRenderingMode=bvcrmDRAFT then begin
  GetDraftMinMaxY;
 end;
 if CurrentRenderingMode in [bvcrmRGB,bvcrmBGR] then begin
  p:=buffercanvas;
  inc(p,CurrentWidthEx*renderminy);
  fillchar(p^,CurrentWidthEx*(rendermaxy-renderminy+1)*sizeof(longword),#0);
 end;
 if HandleUDD and ClearUDDBuffer then begin
  pudd:=buffercanvasudd;
  inc(pudd,CurrentWidthEx*renderminy);
  fillchar(pudd^,CurrentWidthEx*(rendermaxy-renderminy+1)*sizeof(TBeRoVectorCanvasUDDCell),#0);
 end;
 if HandleUDD then begin
  DrawUDDBuffer;
 end;
 if CurrentShape.NumSpans=0 then begin
  if CurrentRenderingMode=bvcrmDRAFT then begin
   MakeDraftSpans;
  end else begin
   SortScanLineCells;
   OptimizeScanLineCells;
   MakeScanLineSpans;
  end;
  SortScanLineSpans;
  ClipScanLineSpans;
  OptimizeScanLineSpans;
 end;
 RenderScanLineSpans;
 case CurrentRenderingMode of
  bvcrmRGB:begin
   TranslateRGBBufferCanvas;
  end;
  bvcrmBGR:begin
   TranslateBGRBufferCanvas;
  end;
 end;
 ResetLocal;
 if CurrentShape=DefaultShape then begin
  ResetShape;
 end;
end;

procedure TBeRoVectorCanvas.GetOrgXY(var x,y:integer);
var tx:integer;
begin
 x:=x*pixelfactor;
 y:=y*pixelfactor;
 tx:=x;
 x:=softtrunc((tx*CurrentInvertedMatrix[0])+(y*CurrentInvertedMatrix[2])+CurrentInvertedMatrix[4]);
 y:=softtrunc((tx*CurrentInvertedMatrix[1])+(y*CurrentInvertedMatrix[3])+CurrentInvertedMatrix[5]);
 x:=x div pixelfactor;
 y:=y div pixelfactor;
end;

procedure TBeRoVectorCanvas.GetOrgXY(x,y:integer;out outx,outy:single);
const divpixelfactor=1/pixelfactor;
begin
 x:=x*pixelfactor;
 y:=y*pixelfactor;
 outx:=((x*CurrentInvertedMatrix[0])+(y*CurrentInvertedMatrix[2])+CurrentInvertedMatrix[4])*divpixelfactor;
 outy:=((x*CurrentInvertedMatrix[1])+(y*CurrentInvertedMatrix[3])+CurrentInvertedMatrix[5])*divpixelfactor;
end;

procedure TBeRoVectorCanvas.ResetMatrix;
begin
 MatrixStackPosition:=-1;
 Matrix:=MatrixIdentity;
end;

procedure TBeRoVectorCanvas.PushMatrix;
begin
 inc(MatrixStackPosition);
 if MatrixStackPosition>=length(MatrixStack) then begin
  setlength(MatrixStack,((MatrixStackPosition+1)+memoryinc) and not memoryincmask);
 end;
 MatrixStack[MatrixStackPosition]:=Matrix;
end;

procedure TBeRoVectorCanvas.PopMatrix;
begin
 if MatrixStackPosition>=0 then begin
  if MatrixStackPosition>=length(MatrixStack) then begin
   setlength(MatrixStack,((MatrixStackPosition+1)+memoryinc) and not memoryincmask);
  end;
  Matrix:=MatrixStack[MatrixStackPosition];
  dec(MatrixStackPosition);
 end;
end;

procedure TBeRoVectorCanvas.CreateShape;
begin
 inc(ShapeStackPosition);
 if ShapeStackPosition>=length(ShapeStack) then begin
  setlength(ShapeStack,((ShapeStackPosition+1)+memoryinc) and not memoryincmask);
 end;
 ShapeStack[ShapeStackPosition]:=CurrentShape;
 CurrentShape:=TBeRoVectorCanvasShape.Create;
 SaveCommands:=CurrentShape<>DefaultShape;
end;

procedure TBeRoVectorCanvas.DestroyShape;
begin
 if assigned(CurrentShape) then begin
  CurrentShape.Destroy;
  CurrentShape:=DefaultShape;
 end;
 if ShapeStackPosition>=0 then begin
  if ShapeStackPosition>=length(ShapeStack) then begin
   setlength(ShapeStack,((ShapeStackPosition+1)+memoryinc) and not memoryincmask);
  end;
  CurrentShape:=ShapeStack[ShapeStackPosition];
  dec(ShapeStackPosition);
 end;
 SaveCommands:=CurrentShape<>DefaultShape;
end;

function TBeRoVectorCanvas.PopShape:TBeRoVectorCanvasShape;
begin
 if CurrentShape=DefaultShape then begin
  result:=nil;
  exit;
 end;
 result:=CurrentShape;
 result.CurrentGamma:=CurrentGamma;
 result.CurrentWinding:=CurrentWinding;
 result.CurrentColor:=CurrentColor;
 result.CurrentHandleUDD:=CurrentHandleUDD;
 result.ClearUDDBuffer:=ClearUDDBuffer;
 result.UDDExtraPixels:=UDDExtraPixels;
 result.CustomColorProc:=CustomColorProc;
 result.CustomColorInstance:=CustomColorInstance;
 result.CurrentWidth:=CurrentWidth;
 result.CurrentHeight:=CurrentHeight;
 result.CurrentRenderingMode:=CurrentRenderingMode;
 result.CurrentMatrix:=Matrix;
 CurrentShape:=DefaultShape;
 if ShapeStackPosition>=0 then begin
  if ShapeStackPosition>=length(ShapeStack) then begin
   setlength(ShapeStack,((ShapeStackPosition+1)+memoryinc) and not memoryincmask);
  end;
  CurrentShape:=ShapeStack[ShapeStackPosition];
  dec(ShapeStackPosition);
 end;
 SaveCommands:=CurrentShape<>DefaultShape;
end;

procedure TBeRoVectorCanvas.DrawShape(Shape:TBeRoVectorCanvasShape);
var OldCurrentShape:TBeRoVectorCanvasShape;
    OldSaveCommands:boolean;
    i:integer;
begin
 OldCurrentShape:=CurrentShape;
 CurrentShape:=Shape;
 DefaultShape.CurrentGamma:=CurrentGamma;
 DefaultShape.CurrentWinding:=CurrentWinding;
 DefaultShape.CurrentColor:=CurrentColor;
 DefaultShape.CurrentHandleUDD:=CurrentHandleUDD;
 DefaultShape.ClearUDDBuffer:=ClearUDDBuffer;
 DefaultShape.UDDExtraPixels:=UDDExtraPixels;
 DefaultShape.CustomColorProc:=CustomColorProc;
 DefaultShape.CustomColorInstance:=CustomColorInstance;
 SetGamma(CurrentShape.CurrentGamma);
 SetWinding(CurrentShape.CurrentWinding);
 SetColor(CurrentShape.CurrentColor);
 SetHandleUDD(CurrentShape.CurrentHandleUDD);
 ClearUDDBuffer:=CurrentShape.ClearUDDBuffer;
 UDDExtraPixels:=CurrentShape.UDDExtraPixels;
 CustomColorProc:=CurrentShape.CustomColorProc;
 CustomColorInstance:=CurrentShape.CustomColorInstance;
 OldSaveCommands:=SaveCommands;
 SaveCommands:=false;
 if (CurrentShape.CurrentWidth<>CurrentWidth) or (CurrentShape.CurrentHeight<>CurrentHeight) or (CurrentShape.CurrentRenderingMode<>CurrentRenderingMode) or (CurrentShape.CurrentMatrix[0]<>Matrix[0]) or (CurrentShape.CurrentMatrix[1]<>Matrix[1]) or (CurrentShape.CurrentMatrix[2]<>Matrix[2]) or (CurrentShape.CurrentMatrix[3]<>Matrix[3]) or (CurrentShape.CurrentMatrix[4]<>Matrix[4]) or (CurrentShape.CurrentMatrix[5]<>Matrix[5]) then begin
  CurrentShape.CurrentWidth:=CurrentWidth;
  CurrentShape.CurrentHeight:=CurrentHeight;
  CurrentShape.CurrentRenderingMode:=CurrentRenderingMode;
  CurrentShape.CurrentMatrix:=Matrix;
  Reset;
  for i:=0 to CurrentShape.NumCommands-1 do begin
   case CurrentShape.Commands[i].CommandType of
    bvcctMOVETO:begin
     with CurrentShape.Commands[i] do begin
      MoveToEx(x,y,a,b,c,d,e,f);
     end;
    end;
    bvcctLINETO:begin
     with CurrentShape.Commands[i] do begin
      LineToEx(x,y,a,b,c,d,e,f);
     end;
    end;
   end;
  end;
 end;
 Draw;
 SaveCommands:=OldSaveCommands;
 SetGamma(DefaultShape.CurrentGamma);
 SetWinding(DefaultShape.CurrentWinding);
 SetColor(DefaultShape.CurrentColor);
 SetHandleUDD(DefaultShape.CurrentHandleUDD);
 ClearUDDBuffer:=DefaultShape.ClearUDDBuffer;
 UDDExtraPixels:=DefaultShape.UDDExtraPixels;
 CustomColorProc:=DefaultShape.CustomColorProc;
 CustomColorInstance:=DefaultShape.CustomColorInstance;
 CurrentShape:=OldCurrentShape;
end;

constructor TBeRoVectorCanvasFont.Create;
begin
 inherited Create;
 Glyphs:=nil;
 KerningTables:=nil;
 fillchar(CharacterMap,sizeof(TBeRoVectorCanvasFontCharacterMap),#0);
 Clear;
end;

destructor TBeRoVectorCanvasFont.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TBeRoVectorCanvasFont.Clear;
var i,j:integer;
begin
 for i:=0 to length(Glyphs)-1 do begin
  for j:=low(Glyphs[i].Styles) to high(Glyphs[i].Styles) do begin
   setlength(Glyphs[i].Styles[j].Polygon.Commands,0);
  end;
 end;
 setlength(Glyphs,0);
 for i:=0 to length(KerningTables)-1 do begin
  setlength(KerningTables[i].KerningPairs,0);
 end;
 setlength(KerningTables,0);
 for i:=0 to length(CharacterMap)-1 do begin
  if assigned(CharacterMap[i]) then begin
   FreeMem(CharacterMap[i]);
   CharacterMap[i]:=nil;
  end;
 end;
 UnitsPerEm:=1;
 UnitsPerPixel:=1;
 AdvanceWidthMax:=0;
 AdvanceHeightMax:=0;
 Styles:=[];
 Size:=12;
end;

function TBeRoVectorCanvasFont.GetCharIndex(CharCode:longword):longword;
var p:PBeRoVectorCanvasFontCharacterMapSub;
begin
 if CharCode<=$10ffff then begin
  p:=CharacterMap[CharCode shr 8];
  if assigned(p) then begin
   result:=p^[CharCode and $ff];
  end else begin
   result:=0;
  end;
 end else begin
  result:=0;
 end;
end;

function TBeRoVectorCanvasFont.Kerning(Left,Right:integer;Horizontal:boolean):integer;
var i,j,beg,mid,endv,idx:integer;
    thiscombined,combined:longword;
begin
 result:=0;
 if length(KerningTables)>0 then begin
  for i:=0 to length(KerningTables)-1 do begin
   if (length(KerningTables[i].KerningPairs)<>0) and (KerningTables[i].Horizontal=Horizontal) then begin
    combined:=(Left shl 16) or Right;
    idx:=-1;
    if KerningTables[i].BinarySearch then begin
     beg:=0;
     endv:=length(KerningTables[i].KerningPairs);
     while beg<=endv do begin
      mid:=(beg+endv) div 2;
      thisCombined:=(KerningTables[i].KerningPairs[mid].left shl 16) or KerningTables[i].KerningPairs[mid].right;
      if combined=thisCombined then begin
       idx:=mid;
       break;
      end;
      if combined<thisCombined then begin
       endv:=mid-1;
      end else begin
       beg:=mid+1;
      end;
     end;
    end else begin
     for j:=0 to length(KerningTables[i].KerningPairs)-1 do begin
      thisCombined:=(KerningTables[i].KerningPairs[j].left shl 16) or KerningTables[i].KerningPairs[j].right;
      if combined=thisCombined then begin
       idx:=j;
       break;
      end;
     end;
    end;
    if idx>=0 then begin
     if KerningTables[i].ValueOverride then begin
      result:=KerningTables[i].KerningPairs[idx].value;
     end else begin
      inc(result,KerningTables[i].KerningPairs[idx].value);
     end;
    end;
   end;
  end;
 end;
end;

function TBeRoVectorCanvasFont.LoadTrueType(var Data;Size:integer;PlatformID:integer=PID_Microsoft;SpecificID:integer=SID_MS_UGL;LanguageID:integer=LID_MS_USEnglish;CollectionIndex:integer=0):boolean;
type TPoint=record
      x,y:integer;
      oncurve:boolean;
     end;
var TrueType:TBeRoVectorCanvasTrueTypeFont;
    i,j,k,g,h,l,m,thinboldstrength,x,y,cx,cy,mx,my,fx,fy,lx,ly,lastx,lasty,direction,xs,ys:integer;
    sum:int64;
    wc:longword;
    Points:array of TPoint;
    pprev,pfirst,pnext,pcur,pin,pout:TPoint;
    ain,aout,ad,s:single;
    Matrix:TBeRoVectorCanvasMatrix;
    oncurve:boolean;
 procedure PutCharIndex(CharCode,CharIndex:longword);
 var p:PBeRoVectorCanvasFontCharacterMapSub;
 begin
  if (CharIndex<>0) and (CharCode<=$10ffff) then begin
   p:=CharacterMap[CharCode shr 8];
   if not assigned(p) then begin
    New(p);
    FillChar(p^,sizeof(TBeRoVectorCanvasFontCharacterMapSub),#0);
    CharacterMap[CharCode shr 8]:=p;
   end;
   p^[CharCode and $ff]:=CharIndex;
  end;
 end;
begin
 Points:=nil;
 result:=false;
 try
  Clear;
  TrueType:=TBeRoVectorCanvasTrueTypeFont.Create(@Data,size,PlatformID,SpecificID,LanguageID,CollectionIndex);
  if TrueType.LastError=TT_ERR_NoError then begin
   AdvanceWidthMax:=TrueType.AdvanceWidthMax;
   AdvanceHeightMax:=TrueType.AdvanceHeightMax;
   UnitsPerEm:=TrueType.unitsPerEm;
   if UnitsPerEm=0 then begin
    UnitsPerEm:=1;
   end;
   UnitsPerPixel:=TrueType.YMax-TrueType.YMin;
   if UnitsPerPixel=0 then begin
    UnitsPerPixel:=1;
   end;
   XMin:=TrueType.XMin;
   YMin:=TrueType.YMin;
   XMax:=TrueType.XMax;
   YMax:=TrueType.YMax;
   thinboldstrength:=UnitsPerEm div 16;
   setlength(Glyphs,length(TrueType.Glyphs));
   for i:=0 to length(TrueType.Glyphs)-1 do begin
    fillchar(Glyphs[i],sizeof(TBeRoVectorCanvasFontGlyph),#0);
    if TrueType.Glyphs[i].UseMetricsFrom>=0 then begin
     j:=TrueType.Glyphs[i].UseMetricsFrom;
     Glyphs[i].AdvanceWidth:=TrueType.Glyphs[j].AdvanceWidth;
     Glyphs[i].LeftSideBearing:=TrueType.Glyphs[j].LeftSideBearing;
     Glyphs[i].RightSideBearing:=TrueType.Glyphs[j].RightSideBearing;
    end else begin
     Glyphs[i].AdvanceWidth:=TrueType.Glyphs[i].AdvanceWidth;
     Glyphs[i].LeftSideBearing:=TrueType.Glyphs[i].LeftSideBearing;
     Glyphs[i].RightSideBearing:=TrueType.Glyphs[i].RightSideBearing;
    end;
    Glyphs[i].AdvanceHeight:=TrueType.Glyphs[i].AdvanceHeight;
    Glyphs[i].TopSideBearing:=TrueType.Glyphs[i].TopSideBearing;
    Glyphs[i].BottomSideBearing:=TrueType.Glyphs[i].BottomSideBearing;
    Glyphs[i].XMin:=TrueType.Glyphs[i].XMin;
    Glyphs[i].YMin:=TrueType.Glyphs[i].YMin;
    Glyphs[i].XMax:=TrueType.Glyphs[i].XMax;
    Glyphs[i].YMax:=TrueType.Glyphs[i].YMax;
    for g:=0 to 5 do begin
     Glyphs[i].Styles[g].AdvanceWidth:=Glyphs[i].AdvanceWidth;
     Glyphs[i].Styles[g].AdvanceHeight:=Glyphs[i].AdvanceHeight;
     Glyphs[i].Styles[g].LeftSideBearing:=Glyphs[i].LeftSideBearing;
     Glyphs[i].Styles[g].RightSideBearing:=Glyphs[i].RightSideBearing;
     Glyphs[i].Styles[g].TopSideBearing:=Glyphs[i].TopSideBearing;
     Glyphs[i].Styles[g].BottomSideBearing:=Glyphs[i].BottomSideBearing;
     Glyphs[i].Styles[g].XMin:=Glyphs[i].XMin;
     Glyphs[i].Styles[g].YMin:=Glyphs[i].YMin;
     Glyphs[i].Styles[g].XMax:=Glyphs[i].XMax;
     Glyphs[i].Styles[g].YMax:=Glyphs[i].YMax;
     if g in [2,3] then begin
      // Thin
      dec(Glyphs[i].Styles[g].AdvanceWidth,thinboldstrength);
      dec(Glyphs[i].Styles[g].LeftSideBearing,thinboldstrength);
      dec(Glyphs[i].Styles[g].RightSideBearing,thinboldstrength);
      dec(Glyphs[i].Styles[g].XMax,thinboldstrength);
     end;
     if g in [4,5] then begin
      // Bold
      inc(Glyphs[i].Styles[g].AdvanceWidth,thinboldstrength);
      inc(Glyphs[i].Styles[g].LeftSideBearing,thinboldstrength);
      inc(Glyphs[i].Styles[g].RightSideBearing,thinboldstrength);
      inc(Glyphs[i].Styles[g].XMax,thinboldstrength);
     end;
    end;
    sum:=0;
    for j:=0 to length(TrueType.Glyphs[i].Contours)-1 do begin
     if length(TrueType.Glyphs[i].Contours[j].Points)=0 then begin
      continue;
     end;
     x:=TrueType.Glyphs[i].Contours[j].Points[0].x;
     y:=TrueType.Glyphs[i].Contours[j].Points[0].y;
     xs:=x;
     ys:=y;
     for k:=1 to length(TrueType.Glyphs[i].Contours[j].Points)-1 do begin
      inc(sum,(x*TrueType.Glyphs[i].Contours[j].Points[k].y)-(y*TrueType.Glyphs[i].Contours[j].Points[k].x));
      x:=TrueType.Glyphs[i].Contours[j].Points[k].x;
      y:=TrueType.Glyphs[i].Contours[j].Points[k].y;
     end;
     inc(sum,(x*ys)-(y*xs));
    end;
    if sum<0 then begin
     direction:=1;
    end else begin
     direction:=-1;
    end;
    for j:=0 to length(TrueType.Glyphs[i].Contours)-1 do begin
     if length(TrueType.Glyphs[i].Contours[j].Points)<2 then begin
      continue;
     end;
     setlength(Points,length(TrueType.Glyphs[i].Contours[j].Points));
     for g:=0 to 5 do begin
      for k:=0 to length(TrueType.Glyphs[i].Contours[j].Points)-1 do begin
       Points[k].oncurve:=TrueType.Glyphs[i].Contours[j].Points[k].OnCurve;
       Points[k].x:=TrueType.Glyphs[i].Contours[j].Points[k].x;
       Points[k].y:=TrueType.Glyphs[i].Contours[j].Points[k].y;
      end;
      if g in [2,3,4,5] then begin
       // Thin/Bold
       pfirst:=Points[0];
       pprev:=Points[length(Points)-1];
       pcur:=pfirst;
       for k:=0 to length(Points)-1 do begin
        if (k+1)<length(Points) then begin
         pnext:=Points[k+1];
        end else begin
         pnext:=pfirst;
        end;
        pin.x:=pcur.x-pprev.x;
        pin.y:=pcur.y-pprev.y;
        pout.x:=pnext.x-pcur.x;
        pout.y:=pnext.y-pcur.y;
        ain:=arctan2(pin.y,pin.x);
        aout:=arctan2(pout.y,pout.x);
        ad:=aout-ain;
        s:=cos(ad*0.5);
        if abs(s)<=0.25 then begin
         pin.x:=0;
         pin.y:=0;
        end else begin
         pin.x:=SoftTrunc((thinboldstrength div 2)/s);
         pin.y:=0;
         if g in [2,3] then begin
          // Thin
          Matrix:=MatrixRotate(((ain+(ad*0.5)-((pi*0.5)*direction))*rad2deg));
         end else begin
          // Bold
          Matrix:=MatrixRotate(((ain+(ad*0.5)+((pi*0.5)*direction))*rad2deg));
         end;
         ApplyMatrixToXY(Matrix,pin.x,pin.y);
        end;
        Points[k].x:=pcur.x+thinboldstrength+pin.x;
        Points[k].y:=pcur.y+thinboldstrength+pin.y;
        pprev:=pcur;
        pcur:=pnext;
       end;
      end;
      if g in [1,3,5] then begin
       // Italic
       for k:=0 to length(Points)-1 do begin
        // Rotate only x coord by about 12 degrees
        Matrix[0]:=1;
        Matrix[1]:=0;
        Matrix[2]:=0.375;
        Matrix[3]:=1;
        Matrix[4]:=0;
        Matrix[5]:=0;
        ApplyMatrixToXY(Matrix,Points[k].x,Points[k].y);
       end;
      end;
      for k:=0 to length(Points)-1 do begin
       Points[k].y:=YMax-Points[k].y;
      end;
      fx:=Points[0].x;
      fy:=Points[0].y;
      lx:=Points[length(Points)-1].x;
      ly:=Points[length(Points)-1].y;
      lastx:=fx;
      lasty:=fy;
      cx:=fx;
      cy:=fx;
      oncurve:=Points[0].oncurve;
      if not oncurve then begin
       if Points[length(Points)-1].oncurve then begin
        lastx:=lx;
        lasty:=ly;
       end else begin
        lastx:=(lastx+lx) div 2;
        lasty:=(lasty+ly) div 2;
        lx:=lastx;
        ly:=lasty;
       end;
      end;
      h:=length(Glyphs[i].Styles[g].Polygon.Commands);
      setlength(Glyphs[i].Styles[g].Polygon.Commands,h+1);
      Glyphs[i].Styles[g].Polygon.Commands[h].CommandType:=bvcfcspctMOVETO;
      Glyphs[i].Styles[g].Polygon.Commands[h].Points[0].x:=fx;
      Glyphs[i].Styles[g].Polygon.Commands[h].Points[0].y:=fy;
      for k:=1 to length(Points)-1 do begin
       x:=Points[k].x;
       y:=Points[k].y;
       if oncurve then begin
        oncurve:=Points[k].oncurve;
        if oncurve then begin
         h:=length(Glyphs[i].Styles[g].Polygon.Commands);
         setlength(Glyphs[i].Styles[g].Polygon.Commands,h+1);
         Glyphs[i].Styles[g].Polygon.Commands[h].CommandType:=bvcfcspctLINETO;
         Glyphs[i].Styles[g].Polygon.Commands[h].Points[0].x:=x;
         Glyphs[i].Styles[g].Polygon.Commands[h].Points[0].y:=y;
        end else begin
         cx:=x;
         cy:=y;
        end;
       end else begin
        oncurve:=Points[k].oncurve;
        if oncurve then begin
         h:=length(Glyphs[i].Styles[g].Polygon.Commands);
         setlength(Glyphs[i].Styles[g].Polygon.Commands,h+1);
         Glyphs[i].Styles[g].Polygon.Commands[h].CommandType:=bvcfcspctCURVETO;
         Glyphs[i].Styles[g].Polygon.Commands[h].Points[0].x:=cx;
         Glyphs[i].Styles[g].Polygon.Commands[h].Points[0].y:=cy;
         Glyphs[i].Styles[g].Polygon.Commands[h].Points[1].x:=x;
         Glyphs[i].Styles[g].Polygon.Commands[h].Points[1].y:=y;
        end else begin
         mx:=(cx+x) div 2;
         my:=(cy+y) div 2;
         h:=length(Glyphs[i].Styles[g].Polygon.Commands);
         setlength(Glyphs[i].Styles[g].Polygon.Commands,h+1);
         Glyphs[i].Styles[g].Polygon.Commands[h].CommandType:=bvcfcspctCURVETO;
         Glyphs[i].Styles[g].Polygon.Commands[h].Points[0].x:=cx;
         Glyphs[i].Styles[g].Polygon.Commands[h].Points[0].y:=cy;
         Glyphs[i].Styles[g].Polygon.Commands[h].Points[1].x:=mx;
         Glyphs[i].Styles[g].Polygon.Commands[h].Points[1].y:=my;
         cx:=x;
         cy:=y;
        end;
       end;
      end;
      if Points[0].oncurve then begin
       if oncurve then begin
        h:=length(Glyphs[i].Styles[g].Polygon.Commands);
        setlength(Glyphs[i].Styles[g].Polygon.Commands,h+1);
        Glyphs[i].Styles[g].Polygon.Commands[h].CommandType:=bvcfcspctLINETO;
        Glyphs[i].Styles[g].Polygon.Commands[h].Points[0].x:=fx;
        Glyphs[i].Styles[g].Polygon.Commands[h].Points[0].y:=fy;
       end else begin
        h:=length(Glyphs[i].Styles[g].Polygon.Commands);
        setlength(Glyphs[i].Styles[g].Polygon.Commands,h+1);
        Glyphs[i].Styles[g].Polygon.Commands[h].CommandType:=bvcfcspctCURVETO;
        Glyphs[i].Styles[g].Polygon.Commands[h].Points[0].x:=cx;
        Glyphs[i].Styles[g].Polygon.Commands[h].Points[0].y:=cy;
        Glyphs[i].Styles[g].Polygon.Commands[h].Points[1].x:=fx;
        Glyphs[i].Styles[g].Polygon.Commands[h].Points[1].y:=fy;
       end;
      end else begin
       if not oncurve then begin
        h:=length(Glyphs[i].Styles[g].Polygon.Commands);
        setlength(Glyphs[i].Styles[g].Polygon.Commands,h+1);
        Glyphs[i].Styles[g].Polygon.Commands[h].CommandType:=bvcfcspctCURVETO;
        Glyphs[i].Styles[g].Polygon.Commands[h].Points[0].x:=cx;
        Glyphs[i].Styles[g].Polygon.Commands[h].Points[0].y:=cy;
        Glyphs[i].Styles[g].Polygon.Commands[h].Points[1].x:=lx;
        Glyphs[i].Styles[g].Polygon.Commands[h].Points[1].y:=ly;
       end;
      end;
     end;
    end;
   end;
   setlength(KerningTables,length(TrueType.KerningTables));
   for i:=0 to length(TrueType.KerningTables)-1 do begin
    KerningTables[i].Horizontal:=TrueType.KerningTables[i].Horizontal;
    KerningTables[i].ValueOverride:=TrueType.KerningTables[i].ValueOverride;
    KerningTables[i].BinarySearch:=TrueType.KerningTables[i].BinarySearch;
    setlength(KerningTables[i].KerningPairs,length(TrueType.KerningTables[i].KerningPairs));
    for j:=0 to length(TrueType.KerningTables[i].KerningPairs)-1 do begin
     KerningTables[i].KerningPairs[j].Left:=TrueType.KerningTables[i].KerningPairs[j].Left;
     KerningTables[i].KerningPairs[j].Right:=TrueType.KerningTables[i].KerningPairs[j].Right;
     KerningTables[i].KerningPairs[j].Value:=TrueType.KerningTables[i].KerningPairs[j].Value;
    end;
   end;
   case TrueType.cmapFormat of
    CMAP_FORMAT0:begin
     for wc:=0 to $ff do begin
      PutCharIndex(wc,TrueType.CharacterMap(wc));
     end;
    end;
    CMAP_FORMAT2,CMAP_FORMAT4,CMAP_FORMAT6:begin
     for wc:=0 to $ffff do begin
      PutCharIndex(wc,TrueType.CharacterMap(wc));
     end;
    end;
    else begin
     for wc:=0 to $10ffff do begin
      PutCharIndex(wc,TrueType.CharacterMap(wc));
     end;
    end;
   end;
   result:=true;
  end else begin
   result:=false;
  end;
  TrueType.Destroy;
 except
  result:=false;
 end;
 setlength(Points,0);
end;

function TBeRoVectorCanvasFont.LoadTrueType(const FileName:ansistring;PlatformID:integer=PID_Microsoft;SpecificID:integer=SID_MS_UGL;LanguageID:integer=LID_MS_USEnglish;CollectionIndex:integer=0):boolean;
var f:file;
    fp:pointer;
    fs,fr:integer;
begin
 result:=false;
 try
  assignfile(f,FileName);
  {$i-}reset(f,1);{$i+}
  if ioresult=0 then begin
   fs:=filesize(f);
   if fs<>0 then begin
    getmem(fp,fs);
    {$i-}blockread(f,fp^,fs,fr);{$i+}
    if (ioresult=0) and (fs=fr) then begin
     result:=LoadTrueType(fp^,fs,PlatformID,SpecificID,LanguageID,CollectionIndex);
    end;
    freemem(fp);
   end;
   closefile(f);
  end;
 except
  result:=false;
 end;
end;

function TBeRoVectorCanvasFont.GetCharCode(const Text:widestring;var Index:integer):longword;
begin
 if (Index>0) and (Index<=length(Text)) then begin
  result:=word(widechar(Text[Index]));
  inc(Index);
  if ((result and $fc00)=$d800) and (Index<=length(Text)) and ((word(widechar(Text[Index])) and $fc00)=$dc00) then begin
   result:=(((result and $3ff) shl 10) or (word(widechar(Text[Index])) and $3ff))+$10000;
   inc(Index);
  end;
 end else begin
  result:=0;
 end;
end;

function TBeRoVectorCanvasFont.TextWidth(const Text:widestring):integer;
var StyleIndex,TextIndex,CurrentGlyph,LastGlyph:integer;
    UnitFactor,UnitScaleFactor:int64;
begin
 result:=0;
 if Size<0 then begin
  UnitFactor:=UnitsPerPixel;
  UnitScaleFactor:=(-Size)*PixelFactor;
 end else begin
  UnitFactor:=UnitsPerEm;
  UnitScaleFactor:=Size*PixelFactor;
 end;
 if bvcfsBOLD in Styles then begin
  if bvcfsITALIC in Styles then begin
   StyleIndex:=5;
  end else begin
   StyleIndex:=4;
  end;
 end else if bvcfsTHIN in Styles then begin
  if bvcfsITALIC in Styles then begin
   StyleIndex:=3;
  end else begin
   StyleIndex:=2;
  end;
 end else begin
  if bvcfsITALIC in Styles then begin
   StyleIndex:=1;
  end else begin
   StyleIndex:=0;
  end;
 end;
 TextIndex:=1;
 LastGlyph:=-1;
 while TextIndex<=length(Text) do begin
  CurrentGlyph:=GetCharIndex(GetCharCode(Text,TextIndex));
  if (CurrentGlyph<0) or (CurrentGlyph>=length(Glyphs)) then begin
   continue;
  end;
  if (LastGlyph>=0) and (LastGlyph<length(Glyphs)) then begin
   inc(result,Kerning(LastGlyph,CurrentGlyph,true));
  end;
  if LastGlyph<0 then begin
   dec(result,Glyphs[CurrentGlyph].Styles[StyleIndex].LeftSideBearing);
  end;
  inc(result,Glyphs[CurrentGlyph].Styles[StyleIndex].AdvanceWidth);
  if result<abs(Glyphs[CurrentGlyph].Styles[StyleIndex].XMax-Glyphs[CurrentGlyph].Styles[StyleIndex].XMin) then begin
   result:=abs(Glyphs[CurrentGlyph].Styles[StyleIndex].XMax-Glyphs[CurrentGlyph].Styles[StyleIndex].XMin);
  end;
  LastGlyph:=CurrentGlyph;
 end;
 result:=((result*UnitScaleFactor)+(UnitFactor div 2)) div UnitFactor;
end;

function TBeRoVectorCanvasFont.TextHeight(const Text:widestring):integer;
var StyleIndex,TextIndex,CurrentGlyph,LastGlyph:integer;
    UnitFactor,UnitScaleFactor:int64;
begin
 result:=0;
 if Size<0 then begin
  UnitFactor:=UnitsPerPixel;
  UnitScaleFactor:=(-Size)*PixelFactor;
 end else begin
  UnitFactor:=UnitsPerEm;
  UnitScaleFactor:=Size*PixelFactor;
 end;
 if bvcfsBOLD in Styles then begin
  if bvcfsITALIC in Styles then begin
   StyleIndex:=5;
  end else begin
   StyleIndex:=4;
  end;
 end else if bvcfsTHIN in Styles then begin
  if bvcfsITALIC in Styles then begin
   StyleIndex:=3;
  end else begin
   StyleIndex:=2;
  end;
 end else begin
  if bvcfsITALIC in Styles then begin
   StyleIndex:=1;
  end else begin
   StyleIndex:=0;
  end;
 end;
 TextIndex:=1;
 LastGlyph:=-1;
 while TextIndex<=length(Text) do begin
  CurrentGlyph:=GetCharIndex(GetCharCode(Text,TextIndex));
  if (CurrentGlyph<0) or (CurrentGlyph>=length(Glyphs)) then begin
   continue;
  end;
  if (LastGlyph>=0) and (LastGlyph<length(Glyphs)) then begin
   inc(result,Kerning(LastGlyph,CurrentGlyph,false));
  end;
  if LastGlyph<0 then begin
   dec(result,Glyphs[CurrentGlyph].Styles[StyleIndex].TopSideBearing);
  end;
  inc(result,Glyphs[CurrentGlyph].Styles[StyleIndex].AdvanceHeight);
  if result<abs(Glyphs[CurrentGlyph].Styles[StyleIndex].YMax-Glyphs[CurrentGlyph].Styles[StyleIndex].YMin) then begin
   result:=abs(Glyphs[CurrentGlyph].Styles[StyleIndex].YMax-Glyphs[CurrentGlyph].Styles[StyleIndex].YMin);
  end;
  LastGlyph:=CurrentGlyph;
 end;
 result:=((result*UnitScaleFactor)+(UnitFactor div 2)) div UnitFactor;
end;

function TBeRoVectorCanvasFont.RowHeight(const Percent:integer):integer;
var UnitFactor,UnitScaleFactor:int64;
begin
 if Size<0 then begin
  UnitFactor:=UnitsPerPixel;
  UnitScaleFactor:=(-Size)*PixelFactor;
 end else begin
  UnitFactor:=UnitsPerEm;
  UnitScaleFactor:=Size*PixelFactor;
 end;
 UnitFactor:=UnitFactor*100;
 UnitScaleFactor:=UnitScaleFactor*Percent;
 result:=((UnitsPerEm*UnitScaleFactor)+(UnitFactor div 2)) div UnitFactor;
end;

procedure TBeRoVectorCanvasFont.Draw(Canvas:TBeRoVectorCanvas;x,y:integer;const Text:widestring;GridFitting:integer=bvcfgfNONE;GridFittingCharwise:boolean=false;tolerance:integer=2;maxlevel:integer=32);
var UnitFactor,UnitScaleFactor:int64;
 procedure ProcessSubpixelFocusAligning(const Text:widestring;StyleIndex:integer);
 const ScaleSize=65536;
       ScaleSizeRadius=ScaleSize shr 8;
       ScaleSizeWidth=ScaleSizeRadius shl 1;
 var CurrentX,CurrentY,LastGlyph,OffsetX,OffsetY,SumDeltaSubpixelX,SumDeltaSubpixelY:integer;
     ScaleX,ScaleY:int64;
  procedure DrawChar(c:longword);
  var CurrentGlyph,CommandIndex,TempX,TempY:integer;
      UnitDivFactor,MulX,MulY:int64;
  begin
   CurrentGlyph:=GetCharIndex(c);
   if (CurrentGlyph>=0) aND (CurrentGlyph<length(Glyphs)) then begin
    UnitDivFactor:=UnitFactor*ScaleSize;
    MulX:=UnitScaleFactor*ScaleX;
    MulY:=UnitScaleFactor*ScaleY;
    if (LastGlyph>=0) and (LastGlyph<length(Glyphs)) then begin
     inc(CurrentX,(Kerning(LastGlyph,CurrentGlyph,true)*MulX) div UnitDivFactor);
     inc(CurrentY,(Kerning(LastGlyph,CurrentGlyph,false)*MulY) div UnitDivFactor);
    end;
    if LastGlyph<0 then begin
     dec(CurrentX,(Glyphs[CurrentGlyph].Styles[StyleIndex].LeftSideBearing*MulX) div UnitDivFactor);
     dec(CurrentY,(Glyphs[CurrentGlyph].Styles[StyleIndex].TopSideBearing*MulY) div UnitDivFactor);
    end;
    TempX:=x+CurrentX+OffsetX;
    TempY:=y+CurrentY+OffsetY;
    for CommandIndex:=0 to length(Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands)-1 do begin
     case Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands[CommandIndex].CommandType of
      bvcfcspctMOVETO:begin
       Canvas.MoveTo(TempX+((Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands[CommandIndex].Points[0].x*MulX) div UnitDivFactor),
                     TempY+((Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands[CommandIndex].Points[0].y*MulY) div UnitDivFactor));
      end;
      bvcfcspctLINETO:begin
       Canvas.LineTo(TempX+((Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands[CommandIndex].Points[0].x*MulX) div UnitDivFactor),
                     TempY+((Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands[CommandIndex].Points[0].y*MulY) div UnitDivFactor));
      end;
      bvcfcspctCURVETO:begin
       Canvas.QuadraticCurveTo(TempX+((Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands[CommandIndex].Points[0].x*MulX) div UnitDivFactor),
                               TempY+((Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands[CommandIndex].Points[0].y*MulY) div UnitDivFactor),
                               TempX+((Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands[CommandIndex].Points[1].x*MulX) div UnitDivFactor),
                               TempY+((Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands[CommandIndex].Points[1].y*MulY) div UnitDivFactor),0,0,0,0,0,0,0,0,0,0,0,0,tolerance,maxlevel);
      end;
     end;
    end;
    inc(CurrentX,(Glyphs[CurrentGlyph].Styles[StyleIndex].AdvanceWidth*MulX) div UnitDivFactor);
    inc(CurrentY,(Glyphs[CurrentGlyph].Styles[StyleIndex].AdvanceHeight*MulY) div UnitDivFactor);
    LastGlyph:=CurrentGlyph;
   end;
  end;
  procedure ScanChar(c:longword;WithOffset:boolean);
  var CurrentGlyph,CommandIndex,TempX,TempY,PointIndex,ResultX,ResultY:integer;
      UnitDivFactor,MulX,MulY:int64;
  begin
   CurrentGlyph:=GetCharIndex(c);
   if (CurrentGlyph>=0) aND (CurrentGlyph<length(Glyphs)) then begin
    UnitDivFactor:=UnitFactor*ScaleSize;
    MulX:=UnitScaleFactor*ScaleX;
    MulY:=UnitScaleFactor*ScaleY;
    if (LastGlyph>=0) and (LastGlyph<length(Glyphs)) then begin
     inc(CurrentX,(Kerning(LastGlyph,CurrentGlyph,true)*MulX) div UnitDivFactor);
     inc(CurrentY,(Kerning(LastGlyph,CurrentGlyph,false)*MulY) div UnitDivFactor);
    end;
    if LastGlyph<0 then begin
     dec(CurrentX,(Glyphs[CurrentGlyph].Styles[StyleIndex].LeftSideBearing*MulX) div UnitDivFactor);
     dec(CurrentY,(Glyphs[CurrentGlyph].Styles[StyleIndex].TopSideBearing*MulY) div UnitDivFactor);
    end;
    if WithOffset then begin
     TempX:=x+CurrentX+OffsetX;
     TempY:=y+CurrentY+OffsetY;
    end else begin
     TempX:=CurrentX;
     TempY:=CurrentY;
    end;
    for CommandIndex:=0 to length(Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands)-1 do begin
     for PointIndex:=0 to ord(Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands[CommandIndex].CommandType=bvcfcspctCURVETO) do begin
      ResultX:=TempX+((Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands[CommandIndex].Points[PointIndex].x*MulX) div UnitDivFactor);
      ResultY:=TempY+((Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands[CommandIndex].Points[PointIndex].y*MulY) div UnitDivFactor);
      inc(SumDeltaSubpixelX,abs(longint((ResultX+128) and $ffffff00)-ResultX));
      inc(SumDeltaSubpixelY,abs(longint((ResultY+128) and $ffffff00)-ResultY));
     end;
    end;
    inc(CurrentX,(Glyphs[CurrentGlyph].Styles[StyleIndex].AdvanceWidth*MulX) div UnitDivFactor);
    inc(CurrentY,(Glyphs[CurrentGlyph].Styles[StyleIndex].AdvanceHeight*MulY) div UnitDivFactor);
    LastGlyph:=CurrentGlyph;
   end;
  end;
  function GetUTF16(c:longword):widestring;
  begin
   if c<=$d7ff then begin
    result:=widechar(word(c));
   end else if c<=$dfff then begin
    result:=#$fffd;
   end else if c<=$fffd then begin
    result:=widechar(word(c));
   end else if c<=$ffff then begin
    result:=#$fffd;
   end else if c<=$10ffff then begin
    dec(c,$10000);
    result:=widechar(word((c shr 10) or $d800));
    result:=result+widechar(word((c and $3ff) or $dc00));
   end else begin
    result:=#$fffd;
   end;
  end;
  procedure FlushCurrentWord(const CurrentWord:widestring);
  var OldLastGlyph,OldCurrentX,OldCurrentY,BestValue,OldValue,Best:integer;
   procedure Scan(WithOffset:boolean);
   var CurrentWordIndex,CurrentChar:integer;
   begin
    LastGlyph:=OldLastGlyph;
    CurrentX:=OldCurrentX;
    CurrentY:=OldCurrentY;
    SumDeltaSubpixelX:=0;
    SumDeltaSubpixelY:=0;
    CurrentWordIndex:=1;
    while CurrentWordIndex<=length(CurrentWord) do begin
     CurrentChar:=GetCharCode(CurrentWord,CurrentWordIndex);
     ScanChar(CurrentChar,WithOffset);
    end;
   end;
  var CurrentWordIndex,CurrentChar,BitIndex:integer;
  begin
   if length(CurrentWord)>0 then begin
    OldLastGlyph:=LastGlyph;
    OldCurrentX:=CurrentX;
    OldCurrentY:=CurrentY;

    OffsetX:=0;
    OffsetY:=0;
    ScaleX:=ScaleSize;
    ScaleY:=ScaleSize;

    Scan(true);
    if (SumDeltaSubpixelX<>0) or (SumDeltaSubpixelY<>0) then begin

     if GridFitting<>bvcfgfY then begin
      Best:=SumDeltaSubpixelX;
      BestValue:=ScaleX;
      for BitIndex:=0 to 8 do begin
       OldValue:=ScaleX;

       ScaleX:=OldValue+(1 shl BitIndex);
       Scan(false);
       if SumDeltaSubpixelX<=Best then begin
        Best:=SumDeltaSubpixelX;
        BestValue:=ScaleX;
       end;

       ScaleX:=OldValue-(1 shl BitIndex);
       Scan(false);
       if SumDeltaSubpixelX<=Best then begin
        Best:=SumDeltaSubpixelX;
        BestValue:=ScaleX;
       end;

       if BestValue=OldValue then begin
        break;
       end;

       ScaleX:=BestValue;
      end;
      ScaleX:=BestValue;
     end;

     if GridFitting<>bvcfgfX then begin
      Best:=SumDeltaSubpixelY;
      BestValue:=ScaleY;
      for BitIndex:=0 to 8 do begin
       OldValue:=ScaleY;

       ScaleY:=OldValue+(1 shl BitIndex);
       Scan(false);
       if SumDeltaSubpixelY<=Best then begin
        Best:=SumDeltaSubpixelY;
        BestValue:=ScaleY;
       end;

       ScaleY:=OldValue-(1 shl BitIndex);
       Scan(false);
       if SumDeltaSubpixelY<=Best then begin
        Best:=SumDeltaSubpixelY;
        BestValue:=ScaleY;
       end;

       if BestValue=OldValue then begin
        break;
       end;

       ScaleY:=BestValue;
      end;
      ScaleY:=BestValue;
     end;

     if GridFitting<>bvcfgfY then begin
      Best:=SumDeltaSubpixelX;
      BestValue:=OffsetX;
      for BitIndex:=0 to 6 do begin
       OldValue:=OffsetX;

       OffsetX:=OldValue-(1 shl BitIndex);
       Scan(true);
       if SumDeltaSubpixelX<=Best then begin
        Best:=SumDeltaSubpixelX;
        BestValue:=OffsetX;
       end;

       OffsetX:=OldValue+(1 shl BitIndex);
       Scan(true);
       if SumDeltaSubpixelX<=Best then begin
        Best:=SumDeltaSubpixelX;
        BestValue:=OffsetX;
       end;

       if BestValue=OldValue then begin
        break;
       end;

       OffsetX:=BestValue;
      end;
      OffsetX:=BestValue;
     end;

     if GridFitting<>bvcfgfX then begin
      Best:=SumDeltaSubpixelY;
      BestValue:=OffsetY;
      for BitIndex:=0 to 6 do begin
       OldValue:=OffsetY;

       OffsetY:=OldValue-(1 shl BitIndex);
       Scan(true);
       if SumDeltaSubpixelY<=Best then begin
        Best:=SumDeltaSubpixelY;
        BestValue:=OffsetY;
       end;

       OffsetY:=OldValue+(1 shl BitIndex);
       Scan(true);
       if SumDeltaSubpixelY<=Best then begin
        Best:=SumDeltaSubpixelY;
        BestValue:=OffsetY;
       end;

       if BestValue=OldValue then begin
        break;
       end;

       OffsetY:=BestValue;
      end;
      OffsetY:=BestValue;
     end;

    end;

    LastGlyph:=OldLastGlyph;
    CurrentX:=OldCurrentX;
    CurrentY:=OldCurrentY;
    CurrentWordIndex:=1;
    while CurrentWordIndex<=length(CurrentWord) do begin
     CurrentChar:=GetCharCode(CurrentWord,CurrentWordIndex);
     DrawChar(CurrentChar);
    end;

    if AdvanceWidthMax<>0 then begin
     inc(CurrentX,OffsetX);
    end;

    if AdvanceHeightMax<>0 then begin
     inc(CurrentY,OffsetY);
    end;

   end;
  end;
 var CurrentWord:widestring;
     TextIndex,CurrentChar:integer;
 begin
  LastGlyph:=-1;
  CurrentWord:='';
  CurrentX:=0;
  CurrentY:=0;
  OffsetX:=0;
  OffsetY:=0;
  ScaleX:=ScaleSize;
  ScaleY:=ScaleSize;
  TextIndex:=1;
  while TextIndex<=length(Text) do begin
   CurrentChar:=GetCharCode(Text,TextIndex);
   if GridFittingCharwise then begin
    FlushCurrentWord(GetUTF16(CurrentChar));
   end else begin
    if ((CurrentChar>=$0009) and (CurrentChar<=$000d)) or
       ((CurrentChar=$0020) or (CurrentChar=$00a0) or (CurrentChar=$1680) or (CurrentChar=$180e)) or
       ((CurrentChar>=$2000) and (CurrentChar<=$200b)) or
       ((CurrentChar=$2028) or (CurrentChar=$2029) or (CurrentChar=$202f) or (CurrentChar=$205f) or (CurrentChar=$3000) or (CurrentChar=$feff) or (CurrentChar=$fffe)) then begin
     FlushCurrentWord(CurrentWord);
     CurrentWord:='';
     OffsetX:=0;
     OffsetY:=0;
     ScaleX:=ScaleSize;
     ScaleY:=ScaleSize;
     DrawChar(CurrentChar);
    end else begin
     CurrentWord:=CurrentWord+GetUTF16(CurrentChar);
    end;
   end;
  end;
  if not GridFittingCharwise then begin
   FlushCurrentWord(CurrentWord);
  end;
 end;
 procedure ProcessUnhinted(const Text:widestring;StyleIndex:integer);
 var TextIndex,CurrentGlyph,LastGlyph,CommandIndex,CurrentX,CurrentY:integer;
 begin
  CurrentX:=0;
  CurrentY:=0;
  TextIndex:=1;
  LastGlyph:=-1;
  while TextIndex<=length(Text) do begin
   CurrentGlyph:=GetCharIndex(GetCharCode(Text,TextIndex));
   if (CurrentGlyph<0) or (CurrentGlyph>=length(Glyphs)) then begin
    continue;
   end;
   if (LastGlyph>=0) and (LastGlyph<length(Glyphs)) then begin
    inc(CurrentX,Kerning(LastGlyph,CurrentGlyph,true));
    inc(CurrentY,Kerning(LastGlyph,CurrentGlyph,false));
   end;
   if LastGlyph<0 then begin
    dec(CurrentX,Glyphs[CurrentGlyph].Styles[StyleIndex].LeftSideBearing);
    dec(CurrentY,Glyphs[CurrentGlyph].Styles[StyleIndex].TopSideBearing);
   end;
   for CommandIndex:=0 to length(Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands)-1 do begin
    case Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands[CommandIndex].CommandType of
     bvcfcspctMOVETO:begin
      Canvas.MoveTo(x+(((CurrentX+Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands[CommandIndex].Points[0].x)*UnitScaleFactor) div UnitFactor),
                    y+(((CurrentY+Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands[CommandIndex].Points[0].y)*UnitScaleFactor) div UnitFactor));
     end;
     bvcfcspctLINETO:begin
      Canvas.LineTo(x+(((CurrentX+Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands[CommandIndex].Points[0].x)*UnitScaleFactor) div UnitFactor),
                    y+(((CurrentY+Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands[CommandIndex].Points[0].y)*UnitScaleFactor) div UnitFactor));
     end;
     bvcfcspctCURVETO:begin
      Canvas.QuadraticCurveTo(x+(((CurrentX+Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands[CommandIndex].Points[0].x)*UnitScaleFactor) div UnitFactor),
                              y+(((CurrentY+Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands[CommandIndex].Points[0].y)*UnitScaleFactor) div UnitFactor),
                              x+(((CurrentX+Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands[CommandIndex].Points[1].x)*UnitScaleFactor) div UnitFactor),
                              y+(((CurrentY+Glyphs[CurrentGlyph].Styles[StyleIndex].Polygon.Commands[CommandIndex].Points[1].y)*UnitScaleFactor) div UnitFactor),0,0,0,0,0,0,0,0,0,0,0,0,tolerance,maxlevel);
     end;
    end;
   end;
   inc(CurrentX,Glyphs[CurrentGlyph].Styles[StyleIndex].AdvanceWidth);
   inc(CurrentY,Glyphs[CurrentGlyph].Styles[StyleIndex].AdvanceHeight);
   LastGlyph:=CurrentGlyph;
  end;
 end;
var StyleIndex:integer;
begin
 if assigned(Canvas) then begin
  if Size<0 then begin
   UnitFactor:=UnitsPerPixel;
   UnitScaleFactor:=(-Size)*PixelFactor;
  end else begin
   UnitFactor:=UnitsPerEm;
   UnitScaleFactor:=Size*PixelFactor;
  end;
  if bvcfsBOLD in Styles then begin
   if bvcfsITALIC in Styles then begin
    StyleIndex:=5;
   end else begin
    StyleIndex:=4;
   end;
  end else if bvcfsTHIN in Styles then begin
   if bvcfsITALIC in Styles then begin
    StyleIndex:=3;
   end else begin
    StyleIndex:=2;
   end;
  end else begin
   if bvcfsITALIC in Styles then begin
    StyleIndex:=1;
   end else begin
    StyleIndex:=0;
   end;
  end;
  if GridFitting<>bvcfgfNONE then begin
   ProcessSubpixelFocusAligning(Text,StyleIndex);
  end else begin
   ProcessUnhinted(Text,StyleIndex);
  end;
 end;
end;

end.

