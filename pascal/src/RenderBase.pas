unit RenderBase;
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

interface

uses SysUtils,Classes,{$ifdef unix}BaseUnix,Unix,UnixType,{$else}Windows,{$endif}Math,{$ifdef mobile}gles20{$else}GL,GLExt{$endif}{$ifdef android},log{$else}{$ifdef win32},logger{$endif}{$endif};

const GLStateTexture:TGLuint=0;

      CANVAS_WIDTH=500;
      CANVAS_HEIGHT=800;

      CANVAS_WIDTH_INV=2/CANVAS_WIDTH;
      CANVAS_HEIGHT_INV=2/CANVAS_HEIGHT;

      SCREEN_WIDTH:integer=480;
      SCREEN_HEIGHT:integer=800;
      SCREEN_BPP=32;

      CollisionQuadTreeDepth=4;

      ExtsChecked:boolean=false;
      CanNPOT:boolean=false;

      tfRGBA8888=0;
      tfRGBA5551=1;
      tfRGBA4444=2;
      tfRGB888=3;
      tfRGB565=4;
      tfALPHA8=5;
      tfLUMINANCE8=6;
      tfLUMINANCEALPHA8=7;

      TextureBytesPerPixel:array[tfRGBA8888..tfLUMINANCEALPHA8] of byte=(4,2,2,3,2,1,1,1);

      TextureFormats:array[tfRGBA8888..tfLUMINANCEALPHA8] of TGLuint=(GL_RGBA,GL_RGBA,GL_RGBA,GL_RGB,GL_RGB,GL_ALPHA,GL_LUMINANCE,GL_LUMINANCE_ALPHA);

      TextureInternalFormats:array[tfRGBA8888..tfLUMINANCEALPHA8] of TGLuint=(GL_RGBA,GL_RGBA,GL_RGBA,GL_RGB,GL_RGB,GL_ALPHA,GL_LUMINANCE,GL_LUMINANCE_ALPHA);

      TextureTypes:array[tfRGBA8888..tfLUMINANCEALPHA8] of TGLuint=(GL_UNSIGNED_BYTE,GL_UNSIGNED_SHORT_5_5_5_1,GL_UNSIGNED_SHORT_4_4_4_4,GL_UNSIGNED_BYTE,GL_UNSIGNED_SHORT_5_6_5,GL_UNSIGNED_BYTE,GL_UNSIGNED_BYTE,GL_UNSIGNED_BYTE);

{$ifdef mobile}
      VertexShaderPrefix=
       '#version 100'+#10+ // OpenGL ES 2.x GLSL-ES 1.00
       '#ifdef GL_FRAGMENT_PRECISION_HIGH'+#10+
         '#define highestp highp'+#10+
       '#else'+#10+
         '#define highestp mediump'+#10+
       '#endif'+#10+
       'precision highp float;'+#10;

      FragmentShaderPrefix=
       '#version 100'+#10+ // OpenGL ES 2.x GLSL-ES 1.00
       '#ifdef GL_FRAGMENT_PRECISION_HIGH'+#10+
         '#define highestp highp'+#10+
       '#else'+#10+
         '#define highestp mediump'+#10+
       '#endif'+#10+
       'precision highestp float;'+#10;

       lowp='lowp ';
       mediump='mediump ';
       highp='highp ';
       highestp='highestp ';
{$else}
      VertexShaderPrefix=
       '#version 130'+#10; // OpenGL 2.x/3.x/4.x GLSL 1.30

      FragmentShaderPrefix=
       '#version 130'+#10; // OpenGL 2.x/3.x/4.x GLSL 1.30

       lowp='';
       mediump='';
       highp='';
       highestp='';
{$endif}

      TextureShaderVertexCode:pchar=
       VertexShaderPrefix+
       'attribute vec2 aPosition;'+#10+
       'uniform '+highp+'vec4 uTexRange;'+#10+
       'uniform '+highp+'vec4 uPosRange;'+#10+
       'varying '+highestp+'vec2 vTexCoord;'+#10+
       'void main(){'+#10+
         'vTexCoord=clamp(uTexRange.xy+(aPosition.xy*uTexRange.zw),0.0,1.0);'+#10+
         'gl_Position=vec4(vec3(vec2(uPosRange.xy+(aPosition.xy*uPosRange.zw)),0.0),1.0);'+#10+
       '}'+#10;

       TextureShaderFragmentCode:pchar=
        FragmentShaderPrefix+
        'uniform '+lowp+'sampler2D uTexUnit0;'+#10+
        'uniform '+highestp+'vec4 uColor;'+#10+
        'varying '+highestp+'vec2 vTexCoord;'+#10+
        'void main(){'+#10+
          'gl_FragColor=clamp(texture2D(uTexUnit0,vTexCoord).xyzw*uColor,0.0,1.0);'+#10+
        '}'+#10;

      SolidShaderVertexCode:pchar=
       VertexShaderPrefix+
       'attribute vec2 aPosition;'+#10+
       'uniform '+highp+'vec4 uPosRange;'+#10+
       'void main(){'+#10+
         'gl_Position=vec4(vec3(vec2(uPosRange.xy+(aPosition.xy*uPosRange.zw)),0.0),1.0);'+#10+
       '}'+#10;

       SolidShaderFragmentCode:pchar=
        FragmentShaderPrefix+
        'uniform '+highestp+'vec4 uColor;'+#10+
        'void main(){'+#10+
          'gl_FragColor=clamp(uColor,0.0,1.0);'+#10+
        '}'+#10;

        QuadVBOPositionData:array[0..(4*2)-1] of TGLfloat=(0,0,0,1,1,0,1,1); // (0,0,1,0,1,1,0,1);

        QuadVBOIndicesData:array[0..3] of TGLushort=(0,1,2,3); // (0,1,2,2,3,0);

        TextureShaderProgram:TGLuint=0;
        TextureShaderVertex:TGLuint=0;
        TextureShaderFragment:TGLuint=0;

        SolidShaderProgram:TGLuint=0;
        SolidShaderVertex:TGLuint=0;
        SolidShaderFragment:TGLuint=0;

        QuadVBOPositions:TGLuint=0;
        QuadVBOIndices:TGLuint=0;

type PCollisionTreeNode=^TCollisionTreeNode;
     TCollisionTreeNode=packed record
      x1,y1,x2,y2,w,h:integer;
      HasContent:boolean;
      Children:array[0..3] of PCollisionTreeNode;
     end;

     PCollisionMap=^TCollisionMap;
     TCollisionMap=record
      Width,Height:integer;
      CollisionTreeRootNode:PCollisionTreeNode;
     end;

     TCollisionMaps=array[boolean] of TCollisionMap;

     TTexture=class
      public
       Data:pointer;
       Width,Height,BPP,Format:integer;
       LinearInterpolating:boolean;
       Repeating:boolean;
       Dirty:boolean;
       TexX,TexY:single;
       Handle:TGLuint;
       CollisionMaps:TCollisionMaps;
       constructor Create(AData:pointer;AWidth,AHeight,AFormat:integer;ALinearInterpolating,ARepeating,AHasCollisionMap:boolean);
       destructor Destroy; override;
       procedure Unload;
       procedure Upload;
       procedure Blit(sx,sy,sw,sh,dx,dy,dz,dw,dh,cr,cg,cb,ca:single);
       function GetCollisionMap(XMirrored:boolean):PCollisionMap;
     end;

     TTextures=array of TTexture;

     TAnimation=class
      public
       Textures:TTextures;
       Count:integer;
       CurrentFrame:integer;
       Position:single;
       Width,Height:integer;
       constructor Create;
       destructor Destroy; override;
       procedure Add(Texture:TTexture);
       procedure Reset;
       procedure Upload;
       procedure Unload;
       procedure SetDirty;
       procedure SetFrame(Frame:integer);
       procedure NextFrame;
       procedure SetFrameTime(t:single;Clamp:boolean=false);
       procedure IncFrameTime(t:single;Clamp:boolean=false);
       procedure Blit(sx,sy,sw,sh,dx,dy,dz,dw,dh,cr,cg,cb,ca:single); overload;
       procedure Blit(dx,dy,dz,dw,dh,cr,cg,cb,ca:single); overload;
       function GetCollisionMap(XMirrored:boolean):PCollisionMap;
     end;

var TextureShaderColor,TextureShaderTexRange,TextureShaderPosRange,TextureShaderTexUnit0:TGLint;
    SolidShaderColor,SolidShaderPosRange:TGLint;

procedure CompileTextureShader;
procedure UnloadTextureShader;

procedure CompileSolidShader;
procedure UnloadSolidShader;

procedure CompileQuadVBO;
procedure UnloadQuadVBO;

function CreateTextureFromPNG(AData:pointer;Size:longword;ALinearInterpolating,ARepeating,AHasCollisionMap:boolean):TTexture;
function Create16BitTextureFromPNG(AData:pointer;Size:longword;ALinearInterpolating,ARepeating,AHasCollisionMap,ADithering:boolean):TTexture;

procedure SwitchToTextureBlit;
procedure SwitchToSolidBlit;
procedure SwitchToNothing;

procedure SolidBlit(dx,dy,dz,dw,dh,cr,cg,cb,ca:single);

procedure QuadVBOBegin;
procedure QuadVBOEnd;

function CreateSolidTexture(Color:longword):TTexture;

procedure DoBlur(Src,Dst:pansichar;Width,Height,Radius:integer);
procedure DoBlurAlpha(Src,Dst:pansichar;Width,Height,Radius:integer);

procedure LogIt(s:string);
procedure DoException(s:string);

function GetNowTime:int64;

function Convert32BitTo16Bit(Data:pointer;Width,Height:integer;Dithering:boolean;var Format:integer):pointer;

function IntersectRect(r1x,r1y,r1w,r1h,r2x,r2y,r2w,r2h:single):boolean;

implementation

uses MathUtils,BeRoPNG;

function GetNowTime:int64;
{$ifdef unix}
var TimeVal:TTimeVal;
    ia,ib:int64;
begin
 fpGetTimeOfDay(@TimeVal,nil);
 ia:=TimeVal.tv_sec*1000;
 ib:=TimeVal.tv_usec div 1000;
 result:=ia+ib;
end;
{$else}
begin
 result:=GetTickCount;
end;
{$endif}

procedure LogIt(s:string);
begin
{$ifdef android}
 Log.__android_log_write(Log.ANDROID_LOG_DEBUG,'libjumpgame',pchar(s));
{$else}
{$ifdef win32}
 logger.Log.LogStatus(s,'Game');
{$endif}
{$endif}
end;

procedure DoException(s:string);
begin
 LogIt(s);
 raise Exception.Create(s);
end;

function LoadShader(ProgramType:TGLuint;Source:pansichar):TGLuint;
var i,Compiled,InfoLogLength:TGLint;
    s:pansichar;
begin
 result:=glCreateShader(ProgramType);
 if result>0 then begin
  i:=length(Source);
	glShaderSource(result,1,@Source,@i);
  glCompileShader(result);
	glGetShaderiv(result,GL_COMPILE_STATUS,@Compiled);
	if Compiled=0 then begin
	 glGetShaderiv(result,GL_INFO_LOG_LENGTH,@InfoLogLength);
	 if InfoLogLength>1 then begin
    GetMem(s,InfoLogLength+1);
    glGetShaderInfoLog(result,InfoLogLength,@InfoLogLength,@s[0]);
    LogIt('Creating the shader didn''t work: '+s);
    raise Exception.Create('Creating the shader didn''t work: '+s);
   end else begin
    LogIt('Creating the shader didn''t work');
    raise Exception.Create('Creating the shader didn''t work');
   end;
  end;
 end else begin
  LogIt('Creating the shader didn''t work');
  raise Exception.Create('Creating the shader didn''t work');
 end;
end;

function CreateProgram(VertexShader,FragmentShader:TGLuint;Position,TexCoord:boolean):TGLuint;
var ProgramLinked,InfoLogLength:TGLint;
    s:pansichar;
begin
 result:=glCreateProgram;
 if result>0 then begin
  glAttachShader(result,VertexShader);
  glAttachShader(result,FragmentShader);
  if Position then begin
   glBindAttribLocation(result,0,pointer(pansichar('aPosition')));
  end;
  if TexCoord then begin
   glBindAttribLocation(result,1,pointer(pansichar('aTexCoord')));
  end;
  glLinkProgram(result);
	glGetProgramiv(result,GL_LINK_STATUS,@ProgramLinked);
	if ProgramLinked=0 then begin
	 glGetProgramiv(result,GL_INFO_LOG_LENGTH,@InfoLogLength);
	 if InfoLogLength>1 then begin
    GetMem(s,InfoLogLength+1);
    glGetProgramInfoLog(result,InfoLogLength,@InfoLogLength,@s[0]);
    LogIt('Creating the shader program didn''t work: '+s);
    raise Exception.Create('Creating the shader program didn''t work: '+s);
   end else begin
    LogIt('Creating the shader program didn''t work');
    raise Exception.Create('Creating the shader program didn''t work');
   end;
  end;
 end else begin
  LogIt('Creating the shader program didn''t work');
  raise Exception.Create('Creating the shader program didn''t work');
 end;
end;

procedure CompileTextureShader;
begin
 UnloadTextureShader;
 try
  TextureShaderVertex:=LoadShader(GL_VERTEX_SHADER,TextureShaderVertexCode);
  TextureShaderFragment:=LoadShader(GL_FRAGMENT_SHADER,TextureShaderFragmentCode);
  TextureShaderProgram:=CreateProgram(TextureShaderVertex,TextureShaderFragment,true,false);
  TextureShaderColor:=glGetUniformLocation(TextureShaderProgram,pointer(pansichar('uColor')));
  TextureShaderTexRange:=glGetUniformLocation(TextureShaderProgram,pointer(pansichar('uTexRange')));
  TextureShaderPosRange:=glGetUniformLocation(TextureShaderProgram,pointer(pansichar('uPosRange')));
  TextureShaderTexUnit0:=glGetUniformLocation(TextureShaderProgram,pointer(pansichar('uTexUnit0')));
  if (TextureShaderColor<0) or (TextureShaderTexRange<0) or (TextureShaderPosRange<0) or (TextureShaderTexUnit0<0) then begin
   raise Exception.Create('Creating the texture shader program didn''t work');
  end;
 except
  raise;
 end;
end;

procedure UnloadTextureShader;
begin
 if TextureShaderVertex<>0 then begin
  glDeleteShader(TextureShaderVertex);
  TextureShaderVertex:=0;
 end;
 if TextureShaderFragment<>0 then begin
  glDeleteShader(TextureShaderFragment);
  TextureShaderFragment:=0;
 end;
 if TextureShaderProgram<>0 then begin
  glDeleteProgram(TextureShaderProgram);
  TextureShaderProgram:=0;
 end;
end;

procedure CompileSolidShader;
begin
 UnloadSolidShader;
 try
  SolidShaderVertex:=LoadShader(GL_VERTEX_SHADER,SolidShaderVertexCode);
  SolidShaderFragment:=LoadShader(GL_FRAGMENT_SHADER,SolidShaderFragmentCode);
  SolidShaderProgram:=CreateProgram(SolidShaderVertex,SolidShaderFragment,true,false);
  SolidShaderColor:=glGetUniformLocation(SolidShaderProgram,pointer(pansichar('uColor')));
  SolidShaderPosRange:=glGetUniformLocation(SolidShaderProgram,pointer(pansichar('uPosRange')));
  if (SolidShaderColor<0) or (SolidShaderPosRange<0) then begin
   raise Exception.Create('Creating the solid shader program didn''t work');
  end;
 except
  raise;
 end;
end;

procedure UnloadSolidShader;
begin
 if SolidShaderVertex<>0 then begin
  glDeleteShader(SolidShaderVertex);
  SolidShaderVertex:=0;
 end;
 if SolidShaderFragment<>0 then begin
  glDeleteShader(SolidShaderFragment);
  SolidShaderFragment:=0;
 end;
 if SolidShaderProgram<>0 then begin
  glDeleteProgram(SolidShaderProgram);
  SolidShaderProgram:=0;
 end;
end;

procedure CompileQuadVBO;
begin
 UnloadQuadVBO;
 begin
  glGenBuffers(1,@QuadVBOPositions);
  glBindBuffer(GL_ARRAY_BUFFER,QuadVBOPositions);
  glBufferData(GL_ARRAY_BUFFER,(2*sizeof(TGLfloat))*4,pointer(@QuadVBOPositionData[0]),GL_STATIC_DRAW);
 end;
 begin
  glGenBuffers(1,@QuadVBOIndices);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,QuadVBOIndices);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER,sizeof(TGLushort)*length(QuadVBOIndicesData),pointer(@QuadVBOIndicesData[0]),GL_STATIC_DRAW);
 end;
end;

procedure UnloadQuadVBO;
begin
 if QuadVBOPositions<>0 then begin
  glDeleteBuffers(1,@QuadVBOPositions);
  QuadVBOPositions:=0;
 end;
 if QuadVBOIndices<>0 then begin
  glDeleteBuffers(1,@QuadVBOIndices);
  QuadVBOIndices:=0;
 end;
end;

function IntersectRect(r1x,r1y,r1w,r1h,r2x,r2y,r2w,r2h:single):boolean;
begin
 result:=((r2x+r2w)>r1x) and ((r2y+r2h)>r1y) and ((r1x+r1w)>r2x) and ((r1y+r1h)>r2y);
end;

procedure SwitchToTextureBlit;
begin
 glUseProgram(TextureShaderProgram);
 glUniform1i(TextureShaderTexUnit0,0);
end;

procedure SwitchToSolidBlit;
begin
 glUseProgram(SolidShaderProgram);
end;

procedure SwitchToNothing;
begin
 glUseProgram(0);
end;

procedure SolidBlit(dx,dy,dz,dw,dh,cr,cg,cb,ca:single);
begin
 glUniform4f(SolidShaderColor,cr,cg,cb,ca);
 glUniform4f(SolidShaderPosRange,(dx*CANVAS_WIDTH_INV)-1,-((dy*CANVAS_HEIGHT_INV)-1),dw*CANVAS_WIDTH_INV,-(dh*CANVAS_HEIGHT_INV));
 glDrawElements(GL_TRIANGLE_STRIP,length(QuadVBOIndicesData),GL_UNSIGNED_SHORT,0);
end;

procedure QuadVBOBegin;
begin
 glBindBuffer(GL_ARRAY_BUFFER,QuadVBOPositions);

 glEnableVertexAttribArray(0);
 glVertexAttribPointer(0,2,GL_FLOAT,GL_FALSE,2*sizeof(TGLfloat),nil);

 glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,QuadVBOIndices);
 glEnableVertexAttribArray(0);
end;

procedure QuadVBOEnd;
begin
 glDisableVertexAttribArray(0);
end;

function Convert32BitTo16Bit(Data:pointer;Width,Height:integer;Dithering:boolean;var Format:integer):pointer;
 procedure Dither(sr,sg,sb,sa:byte);
 const Matrix:array[0..1,0..2] of integer=((0,0,7),(3,5,1));
       Divisor=16;
       OffsetX=-1;
       OffsetY=0;
 type PRGBA=^TRGBA;
      TRGBA=record
       r,g,b,a:byte;
      end;
      TColorDifference=record
       r,g,b,a:integer;
      end;
 var x,y,i,j,xx,yy,Coeff:integer;
     SrcPixel:PRGBA;
     DestPixel:TRGBA;
     Difference:TColorDifference;
  function clamp(c:integer):integer;
  begin
   if c<0 then begin
    result:=0;
   end else if c>255 then begin
    result:=255;
   end else begin
    result:=c;
   end;
  end;
 begin
  for y:=0 to Height-1 do begin
   for x:=0 to Width-1 do begin
    SrcPixel:=PRGBA(pointer(@pansichar(Data)[((y*Width)+x)*4]));
    DestPixel.r:=(SrcPixel^.r shr sr) shl sr;
    DestPixel.g:=(SrcPixel^.g shr sg) shl sg;
    DestPixel.b:=(SrcPixel^.b shr sb) shl sb;
    DestPixel.a:=(SrcPixel^.a shr sa) shl sa;
    Difference.r:=SrcPixel^.r-DestPixel.r;
    Difference.g:=SrcPixel^.g-DestPixel.g;
    Difference.b:=SrcPixel^.b-DestPixel.b;
    Difference.a:=SrcPixel^.a-DestPixel.a;
    SrcPixel^:=DestPixel;
    if (Difference.r<>0) or (Difference.g<>0) or (Difference.b<>0) or (Difference.a<>0) then begin
     for j:=low(Matrix) to high(Matrix) do begin
      for i:=low(Matrix[j]) to high(Matrix[j]) do begin
       Coeff:=Matrix[j,i];
       if Coeff<>0 then begin
        xx:=(x+i)+OffsetX;
        yy:=(y+j)+OffsetY;
        if ((xx>=0) and (xx<Width)) and ((yy>=0) and (yy<Height)) then begin
         SrcPixel:=pointer(@pansichar(Data)[((yy*Width)+xx)*4]);
         SrcPixel^.r:=clamp(SrcPixel^.r+((Coeff*Difference.r) div Divisor));
         SrcPixel^.g:=clamp(SrcPixel^.g+((Coeff*Difference.g) div Divisor));
         SrcPixel^.b:=clamp(SrcPixel^.b+((Coeff*Difference.b) div Divisor));
         SrcPixel^.a:=clamp(SrcPixel^.a+((Coeff*Difference.a) div Divisor));
        end;
       end;
      end;
     end;
    end;
   end;
  end;
 end;
var i:integer;
    Alpha:boolean;
    pa,pb:pansichar;
    w:word;
begin
 GetMem(result,Width*Height*2);
 Alpha:=false;
 for i:=0 to (Width*Height)-1 do begin
  Alpha:=Alpha or (byte(pansichar(Data)[(i*4)+3])<>255);
  if Alpha then begin
   break;
  end;
 end;
 if Alpha then begin
  if Dithering then begin
   Dither(4,4,4,4);
  end;
  for i:=0 to (Width*Height)-1 do begin
   pa:=@pansichar(Data)[i*4];
   pb:=@pansichar(result)[i*2];
   w:=((byte(pa[0]) shr 4) shl 12) or ((byte(pa[1]) shr 4) shl 8) or ((byte(pa[2]) shr 4) shl 4) or ((byte(pa[3]) shr 4) shl 0);
   byte(pb[0]):=w and $ff;
   byte(pb[1]):=w shr 8;
  end;
  Format:=tfRGBA4444;
 end else begin
  if Dithering then begin
   Dither(3,2,3,0);
  end;
  for i:=0 to (Width*Height)-1 do begin
   pa:=@pansichar(Data)[i*4];
   pb:=@pansichar(result)[i*2];
   w:=((byte(pa[0]) shr 3) shl 11) or ((byte(pa[1]) shr 2) shl 5) or ((byte(pa[2]) shr 3) shl 0);
   byte(pb[0]):=w and $ff;
   byte(pb[1]):=w shr 8;
  end;
  Format:=tfRGB565;
 end;
 FreeMem(Data);
end;

function CreateTextureFromPNG(AData:pointer;Size:longword;ALinearInterpolating,ARepeating,AHasCollisionMap:boolean):TTexture;
var Data,NewData:pointer;
    Width,Height,i:integer;
    Alpha:boolean;
begin
 if LoadPNG(AData,Size,Data,Width,Height) then begin
  Alpha:=false;
  for i:=0 to (Width*Height)-1 do begin
   Alpha:=Alpha or (byte(pansichar(Data)[(i*4)+3])<>255);
   if Alpha then begin
    break;
   end;
  end;
  if Alpha then begin
   result:=TTexture.Create(Data,Width,Height,tfRGBA8888,ALinearInterpolating,ARepeating,AHasCollisionMap);
  end else begin
   GetMem(NewData,Width*Height*3);
   for i:=0 to (Width*Height)-1 do begin
    Move(pansichar(Data)[i*4],pansichar(NewData)[i*3],3);
   end;
   FreeMem(Data);
   result:=TTexture.Create(NewData,Width,Height,tfRGB888,ALinearInterpolating,ARepeating,AHasCollisionMap);
  end;
 end else begin
  result:=nil;
 end;
end;

function Create16BitTextureFromPNG(AData:pointer;Size:longword;ALinearInterpolating,ARepeating,AHasCollisionMap,ADithering:boolean):TTexture;
var Data:pointer;
    Width,Height,Format:integer;
begin
 if LoadPNG(AData,Size,Data,Width,Height) then begin
  Data:=Convert32BitTo16Bit(Data,Width,Height,ADithering,Format);
  result:=TTexture.Create(Data,Width,Height,Format,ALinearInterpolating,ARepeating,AHasCollisionMap);
 end else begin
  result:=nil;
 end;
end;

constructor TTexture.Create(AData:pointer;AWidth,AHeight,AFormat:integer;ALinearInterpolating,ARepeating,AHasCollisionMap:boolean);
type TMap=array of bytebool;
var i,x,y:integer;
 function BuildCollisionTreeNode(const Map:TMap;x1,y1,x2,y2:integer;RemainDepth:integer=64):PCollisionTreeNode;
 var x,y,xm,ym,i:integer;
     c:array[0..3] of boolean;
 begin
  New(result);
  result^.x1:=x1;
  result^.y1:=y1;
  result^.x2:=x2;
  result^.y2:=y2;
  result^.HasContent:=false;
  if (RemainDepth>0) and ((abs(result^.x2-result^.x1)>=2) and (abs(result^.y2-result^.y1)>=2)) then begin
   xm:=((result^.x1+result^.x2)+1) div 2;
   ym:=((result^.y1+result^.y2)+1) div 2;
   result^.Children[0]:=BuildCollisionTreeNode(Map,result^.x1,result^.y1,xm,ym,RemainDepth-1);
   result^.Children[1]:=BuildCollisionTreeNode(Map,xm,result^.y1,result^.x2,ym,RemainDepth-1);
   result^.Children[2]:=BuildCollisionTreeNode(Map,result^.x1,ym,xm,result^.y2,RemainDepth-1);
   result^.Children[3]:=BuildCollisionTreeNode(Map,xm,ym,result^.x2,result^.y2,RemainDepth-1);
   for i:=0 to 3 do begin
    c[i]:=assigned(result^.Children[i]) and result^.Children[i]^.HasContent;
   end;
   if c[0] and c[1] and c[2] and c[3] then begin
    for i:=0 to 3 do begin
     Dispose(result^.Children[i]);
     result^.Children[i]:=nil;
    end;
    result^.HasContent:=true;
   end else begin
    if c[0] and c[1] then begin
     result^.Children[0]^.x2:=result^.Children[1]^.x2;
     result^.Children[0]^.w:=result^.w;
     Dispose(result^.Children[1]);
     result^.Children[1]:=nil;
     c[0]:=false;
     c[1]:=false;
    end;
    if c[2] and c[3] then begin
     result^.Children[2]^.x2:=result^.Children[3]^.x2;
     result^.Children[2]^.w:=result^.w;
     Dispose(result^.Children[3]);
     result^.Children[3]:=nil;
     c[2]:=false;
     c[3]:=false;
    end;
    if c[0] and c[2] then begin
     result^.Children[0]^.y2:=result^.Children[2]^.y2;
     result^.Children[0]^.h:=result^.h;
     Dispose(result^.Children[2]);
     result^.Children[2]:=nil;
     c[0]:=false;
     c[2]:=false;
    end;
    if c[1] and c[3] then begin
     result^.Children[1]^.y2:=result^.Children[3]^.y2;
     result^.Children[1]^.h:=result^.h;
     Dispose(result^.Children[3]);
     result^.Children[3]:=nil;
     c[1]:=false;
     c[3]:=false;
    end;
    for i:=0 to 3 do begin
     if assigned(result^.Children[i]) and not (result^.Children[i]^.HasContent or (assigned(result^.Children[i]^.Children[0]) or assigned(result^.Children[i]^.Children[1]) or assigned(result^.Children[i]^.Children[2]) or assigned(result^.Children[i]^.Children[3]))) then begin
      Dispose(result^.Children[i]);
      result^.Children[i]:=nil;
     end;
    end;
   end;
  end else begin
   result^.Children[0]:=nil;
   result^.Children[1]:=nil;
   result^.Children[2]:=nil;
   result^.Children[3]:=nil;
   for y:=result^.y1 to result^.y2-1 do begin
    for x:=result^.x1 to result^.x2-1 do begin
     if Map[(y*Width)+x] then begin
      result^.HasContent:=true;
      break;
     end;
    end;
   end;
  end;
 end;
 procedure FinalizeCollisionTreeNode(n:PCollisionTreeNode);
 var i:integer;
 begin
  if assigned(n) then begin
   n^.w:=n^.x2-n^.x1;
   n^.h:=n^.y2-n^.y1;
   for i:=0 to 3 do begin
    if assigned(n^.Children[i]) then begin
     FinalizeCollisionTreeNode(n^.Children[i]);
    end;
   end;
  end;
 end;
var Map,MapMirrored:TMap;
begin
 inherited Create;
 Data:=AData;
 Width:=AWidth;
 Height:=AHeight;
 Format:=AFormat;
 BPP:=TextureBytesPerPixel[Format];
 if not assigned(Data) then begin
  GetMem(Data,Width*Height*BPP);
 end;
 FillChar(CollisionMaps,sizeof(TCollisionMaps),#0);
 LinearInterpolating:=ALinearInterpolating;
 Repeating:=ARepeating;
 Dirty:=true;
 Handle:=0;
 GLStateTexture:=$ffffffff;
 Map:=nil;
 MapMirrored:=nil;
 try
  if AHasCollisionMap then begin
   CollisionMaps[false].Width:=Width;
   CollisionMaps[false].Height:=Height;
   CollisionMaps[true].Width:=Width;
   CollisionMaps[true].Height:=Height;
   SetLength(Map,Width*Height);
   SetLength(MapMirrored,Width*Height);
   case Format of
    tfRGBA8888:begin
     for i:=0 to (Width*Height)-1 do begin
      Map[i]:=byte(pansichar(Data)[(i*4)+3])<>0;
     end;
    end;
    tfRGBA5551:begin
     for i:=0 to (Width*Height)-1 do begin
      Map[i]:=(byte(pansichar(Data)[i*2]) and 1)<>0;
     end;
    end;
    tfRGBA4444:begin
     for i:=0 to (Width*Height)-1 do begin
      Map[i]:=(byte(pansichar(Data)[i*2]) and $f)<>0;
     end;
    end;
    tfRGB888:begin
     for i:=0 to (Width*Height)-1 do begin
      Map[i]:=longword(pointer(@pansichar(Data)[i*4]))<>0;
     end;
    end;
    tfRGB565:begin
     for i:=0 to (Width*Height)-1 do begin
      Map[i]:=word(pointer(@pansichar(Data)[i*2]))<>0;
     end;
    end;
    tfALPHA8:begin
     for i:=0 to (Width*Height)-1 do begin
      Map[i]:=byte(pansichar(Data)[i])<>0;
     end;
    end;
    tfLUMINANCE8:begin
     for i:=0 to (Width*Height)-1 do begin
      Map[i]:=byte(pansichar(Data)[i])<>0;
     end;
    end;
    tfLUMINANCEALPHA8:begin
     for i:=0 to (Width*Height)-1 do begin
      Map[i]:=word(pointer(@pansichar(Data)[i*2]))<>0;
     end;
    end;
   end;
   for y:=0 to Height-1 do begin
    for x:=0 to Width-1 do begin
     MapMirrored[(y*Width)+(Width-(x+1))]:=Map[(y*Width)+x];
    end;
   end;
   CollisionMaps[false].CollisionTreeRootNode:=BuildCollisionTreeNode(Map,0,0,Width,Height);
   CollisionMaps[true].CollisionTreeRootNode:=BuildCollisionTreeNode(MapMirrored,0,0,Width,Height);
   FinalizeCollisionTreeNode(CollisionMaps[false].CollisionTreeRootNode);
   FinalizeCollisionTreeNode(CollisionMaps[true].CollisionTreeRootNode);
  end;
 finally
  SetLength(Map,0);
  SetLength(MapMirrored,0);
 end;
end;

destructor TTexture.Destroy;
 procedure FreeCollisionTreeNode(var n:PCollisionTreeNode);
 var i:integer;
 begin
  if assigned(n) then begin
   for i:=0 to 3 do begin
    if assigned(n^.Children[i]) then begin
     FreeCollisionTreeNode(n^.Children[i]);
     n^.Children[i]:=nil;
    end;
   end;
   n:=nil;
  end;
 end;
begin
 Unload;
 if assigned(Data) then begin
  FreeMem(Data);
  Data:=nil;
 end;
 FreeCollisionTreeNode(CollisionMaps[false].CollisionTreeRootNode);
 FreeCollisionTreeNode(CollisionMaps[true].CollisionTreeRootNode);
 inherited Destroy;
end;

procedure TTexture.Unload;
begin
 if Handle<>0 then begin
  glDeleteTextures(1,@Handle);
  Handle:=0;
  Dirty:=true;
 end;
 GLStateTexture:=$ffffffff;
end;

procedure TTexture.Upload;
begin
 if Dirty then begin
  Unload;
  TexX:=1/Width;
  TexY:=1/Height;
  glGenTextures(1,@Handle);
  glBindTexture(GL_TEXTURE_2D,Handle);
  if LinearInterpolating then begin
   glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
   glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
  end else begin
   glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
   glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
  end;
  if Repeating then begin
   glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_REPEAT);
   glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_REPEAT);
  end else begin
   glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
   glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE);
  end;
  glTexImage2D(GL_TEXTURE_2D,0,TextureInternalFormats[Format],Width,Height,0,TextureFormats[Format],TextureTypes[Format],Data);
  Dirty:=false;
  GLStateTexture:=$ffffffff;
 end;
end;

procedure TTexture.Blit(sx,sy,sw,sh,dx,dy,dz,dw,dh,cr,cg,cb,ca:single);
begin
 if Dirty then begin
  Upload;
  Dirty:=false;
 end;
 if not Dirty then begin
  if GLStateTexture<>Handle then begin
   GLStateTexture:=Handle;
   glBindTexture(GL_TEXTURE_2D,Handle);
  end;
  glUniform4f(TextureShaderColor,cr,cg,cb,ca);
  glUniform4f(TextureShaderPosRange,(dx*CANVAS_WIDTH_INV)-1,-((dy*CANVAS_HEIGHT_INV)-1),dw*CANVAS_WIDTH_INV,-(dh*CANVAS_HEIGHT_INV));
  glUniform4f(TextureShaderTexRange,sx*TexX,sy*TexY,sw*TexX,sh*TexY);
  glDrawElements(GL_TRIANGLE_STRIP,length(QuadVBOIndicesData),GL_UNSIGNED_SHORT,0);
 end;
end;

function TTexture.GetCollisionMap(XMirrored:boolean):PCollisionMap;
begin
 result:=@CollisionMaps[XMirrored];
end;

function CreateSolidTexture(Color:longword):TTexture;
var p:pointer;
    pp:^byte;
    i:integer;
begin
 GetMem(p,4*4*4);
 for i:=1 to 4*4 do begin
  pp:=p;
  pp^:=Color and $ff;
  inc(pp);
  pp^:=(Color shr 8) and $ff;
  inc(pp);
  pp^:=(Color shr 16) and $ff;
  inc(pp);
  pp^:=(Color shr 24) and $ff;
  inc(pp);
 end;
 result:=TTexture.Create(p,4,4,tfRGBA8888,false,false,false);
end;

procedure DoBlur(Src,Dst:pansichar;Width,Height,Radius:integer);
var Kernel:array of array of single;
    KernelSize,x,y,i,j,k:integer;
    KernelSum,KernelSumFactor,r,g,b,a,f:single;
 procedure BuildKernel(sigma:integer);
 var ss,Factor,f0,f1,f2,v:single;
     i,j:integer;
 begin
  ss:=sqr(sigma);
  Factor:=2*pi*ss;
  f0:=1.0/Factor;
  f1:=1.0/(Factor*2);
  f2:=1.0/(ss*2);
  KernelSize:=16;
  for i:=0 to 15 do begin
   if (exp(-sqr(i)/(2*ss))/Factor)<1e-3 then begin
    KernelSize:=i;
    break;
   end;
  end;
  SetLength(Kernel,KernelSize,KernelSize);
  j:=0;
  for i:=0 to KernelSize-1 do begin
   v:=exp(-sqr(i)*f2)*f0;
   Kernel[j,i]:=v;
  end;
  for j:=1 to KernelSize-1 do begin
   for i:=0 to KernelSize-1 do begin
    v:=exp(-(sqr(i)+sqr(j))*f2)*f1;
    Kernel[j,i]:=v;
   end;
  end;
  KernelSum:=0;
  for j:=1-KernelSize to KernelSize-1 do begin
   for i:=1-KernelSize to KernelSize-1 do begin
    KernelSum:=KernelSum+Kernel[abs(j),abs(i)];
   end;
  end;
  KernelSumFactor:=1.0/KernelSum;
 end;
begin
 Kernel:=nil;
 try
  BuildKernel(Radius);
  for y:=0 to Height-1 do begin
   for x:=0 to Width-1 do begin
    r:=0;
    g:=0;
    b:=0;
    a:=0;
    for j:=1-KernelSize to KernelSize-1 do begin
     if ((y+j)>=0) and ((y+j)<Height) then begin
      for i:=1-KernelSize to KernelSize-1 do begin
       if ((x+i)>=0) and ((x+i)<Width) then begin
        k:=(((y+j)*Width)+(x+i))*4;
        f:=Kernel[abs(j),abs(i)];
        r:=r+byte(pansichar(Src)[k+0])*f;
        g:=g+byte(pansichar(Src)[k+1])*f;
        b:=b+byte(pansichar(Src)[k+2])*f;
        a:=a+byte(pansichar(Src)[k+3])*f;
       end;
      end;
     end;
    end;
    k:=((y*Width)+x)*4;
    byte(pansichar(Dst)[k+0]):=min(max(round(r*KernelSumFactor),0),255);
    byte(pansichar(Dst)[k+1]):=min(max(round(g*KernelSumFactor),0),255);
    byte(pansichar(Dst)[k+2]):=min(max(round(b*KernelSumFactor),0),255);
    byte(pansichar(Dst)[k+3]):=min(max(round(a*KernelSumFactor),0),255);
   end;
  end;
 finally
  SetLength(Kernel,0,0);
 end;
end;

procedure DoBlurAlpha(Src,Dst:pansichar;Width,Height,Radius:integer);
var Kernel:array of array of single;
    KernelSize,x,y,i,j,k:integer;
    KernelSum,KernelSumFactor,a,f:single;
 procedure BuildKernel(sigma:integer);
 var ss,Factor,f0,f1,f2,v:single;
     i,j:integer;
 begin
  ss:=sqr(sigma);
  Factor:=2*pi*ss;
  f0:=1.0/Factor;
  f1:=1.0/(Factor*2);
  f2:=1.0/(ss*2);
  KernelSize:=16;
  for i:=0 to 15 do begin
   if (exp(-sqr(i)/(2*ss))/Factor)<1e-3 then begin
    KernelSize:=i;
    break;
   end;
  end;
  SetLength(Kernel,KernelSize,KernelSize);
  j:=0;
  for i:=0 to KernelSize-1 do begin
   v:=exp(-sqr(i)*f2)*f0;
   Kernel[j,i]:=v;
  end;
  for j:=1 to KernelSize-1 do begin
   for i:=0 to KernelSize-1 do begin
    v:=exp(-(sqr(i)+sqr(j))*f2)*f1;
    Kernel[j,i]:=v;
   end;
  end;
  KernelSum:=0;
  for j:=1-KernelSize to KernelSize-1 do begin
   for i:=1-KernelSize to KernelSize-1 do begin
    KernelSum:=KernelSum+Kernel[abs(j),abs(i)];
   end;
  end;
  KernelSumFactor:=1.0/KernelSum;
 end;
begin
 Kernel:=nil;
 try
  BuildKernel(Radius);
  for y:=0 to Height-1 do begin
   for x:=0 to Width-1 do begin
    a:=0;
    for j:=1-KernelSize to KernelSize-1 do begin
     if ((y+j)>=0) and ((y+j)<Height) then begin
      for i:=1-KernelSize to KernelSize-1 do begin
       if ((x+i)>=0) and ((x+i)<Width) then begin
        k:=(((y+j)*Width)+(x+i))*4;
        f:=Kernel[abs(j),abs(i)];
        a:=a+byte(pansichar(Src)[k+3])*f;
       end;
      end;
     end;
    end;
    k:=((y*Width)+x)*4;
    byte(pansichar(Dst)[k+0]):=byte(pansichar(Src)[k+0]);
    byte(pansichar(Dst)[k+1]):=byte(pansichar(Src)[k+1]);
    byte(pansichar(Dst)[k+2]):=byte(pansichar(Src)[k+2]);
    byte(pansichar(Dst)[k+3]):=min(max(round(a*KernelSumFactor),0),255);
   end;
  end;
 finally
  SetLength(Kernel,0,0);
 end;
end;

constructor TAnimation.Create;
begin
 inherited Create;
 Textures:=nil;
 Count:=0;
 CurrentFrame:=0;
 Position:=0;
 Width:=0;
 Height:=0;
end;

destructor TAnimation.Destroy;
var i:integer;
begin
 for i:=0 to Count-1 do begin
  FreeAndNil(Textures[i]);
 end;
 SetLength(Textures,0);
 inherited Destroy;
end;

procedure TAnimation.Add(Texture:TTexture);
var i,j:integer;
begin
 i:=Count;
 inc(Count);
 j:=length(Textures);
 if Count>=j then begin
  if j<1 then begin
   j:=1;
  end;
  while Count>=j do begin
   inc(j,j);
  end;
  SetLength(Textures,j);
 end;
 Textures[i]:=Texture;
 Width:=max(Width,Texture.Width);
 Height:=max(Height,Texture.Height);
end;

procedure TAnimation.Reset;
begin
 CurrentFrame:=0;
 Position:=0;
end;

procedure TAnimation.Upload;
var i:integer;
begin
 for i:=0 to Count-1 do begin
  Textures[i].Upload;
 end;
end;

procedure TAnimation.Unload;
var i:integer;
begin
 for i:=0 to Count-1 do begin
  Textures[i].Unload;
 end;
end;

procedure TAnimation.SetDirty;
var i:integer;
begin
 for i:=0 to Count-1 do begin
  Textures[i].Dirty:=true;
 end;
end;

procedure TAnimation.SetFrame(Frame:integer);
begin
 CurrentFrame:=Frame;
 if Count>0 then begin
  while CurrentFrame<0 do begin
   inc(CurrentFrame,Count);
  end;
  while CurrentFrame>=Count do begin
   dec(CurrentFrame,Count);
  end;
 end;
end;

procedure TAnimation.NextFrame;
begin
 SetFrame(CurrentFrame+1);
end;

procedure TAnimation.SetFrameTime(t:single;Clamp:boolean=false);
begin
 if Clamp then begin
  if Count>0 then begin
   Position:=min(max(t,0),(Count-1)/Count);
  end else begin
   Position:=min(max(t,0),1);
  end;
 end else begin
  Position:=frac(t);
  while Position<0 do begin
   Position:=Position+1;
  end;
  while Position>=1 do begin
   Position:=Position-1;
  end;
 end;
 SetFrame(trunc(Position*Count));
end;

procedure TAnimation.IncFrameTime(t:single;Clamp:boolean=false);
begin
 SetFrameTime(Position+t,Clamp);
end;

procedure TAnimation.Blit(sx,sy,sw,sh,dx,dy,dz,dw,dh,cr,cg,cb,ca:single);
begin
 if Count>0 then begin
  while CurrentFrame<0 do begin
   inc(CurrentFrame,Count);
  end;
  while CurrentFrame>=Count do begin
   dec(CurrentFrame,Count);
  end;
  Textures[CurrentFrame].Blit(sx,sy,sw,sh,dx,dy,dz,dw,dh,cr,cg,cb,ca);
 end;
end;

procedure TAnimation.Blit(dx,dy,dz,dw,dh,cr,cg,cb,ca:single);
begin
 if Count>0 then begin
  while CurrentFrame<0 do begin
   inc(CurrentFrame,Count);
  end;
  while CurrentFrame>=Count do begin
   dec(CurrentFrame,Count);
  end;
  Textures[CurrentFrame].Blit(0,0,Textures[CurrentFrame].Width,Textures[CurrentFrame].Height,dx,dy,dz,dw,dh,cr,cg,cb,ca);
 end;
end;

function TAnimation.GetCollisionMap(XMirrored:boolean):PCollisionMap;
begin
 if Count>0 then begin
  while CurrentFrame<0 do begin
   inc(CurrentFrame,Count);
  end;
  while CurrentFrame>=Count do begin
   dec(CurrentFrame,Count);
  end;
  result:=Textures[CurrentFrame].GetCollisionMap(XMirrored);
 end else begin
  result:=nil;
 end;
end;

end.
