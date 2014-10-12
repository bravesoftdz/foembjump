UNIT BeRoStringTree;
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

CONST MaxStringHashes=32;

TYPE TBeRoStringTreeLink=LONGWORD;

     PBeRoStringTreeNode=^TBeRoStringTreeNode;
     TBeRoStringTreeNode=RECORD
      TheChar:CHAR;
      Link:TBeRoStringTreeLink;
      LinkExist:BOOLEAN;
      Prevoius,Next,Up,Down:PBeRoStringTreeNode;
     END;

     TBeRoStringHashes=ARRAY[0..MaxStringHashes-1] OF LONGWORD;
     TBeRoStringHashStrings=ARRAY[0..MaxStringHashes-1] OF STRING;
     TBeRoStringHashNodes=ARRAY[0..MaxStringHashes-1] OF PBeRoStringTreeNode;

     TBeRoStringTree=CLASS
      PRIVATE
       Root:PBeRoStringTreeNode;
       Hashes:TBeRoStringHashes;
       HashStrings:TBeRoStringHashStrings;
       HashNodes:TBeRoStringHashNodes;
      PUBLIC
       Hashing:BOOLEAN;
       CONSTRUCTOR Create;
       DESTRUCTOR Destroy; OVERRIDE;
       PROCEDURE Clear;
       PROCEDURE Dump;
       FUNCTION Add(Content:STRING;Link:TBeRoStringTreeLink;Replace:BOOLEAN=FALSE):BOOLEAN;
       FUNCTION Delete(Content:STRING):BOOLEAN;
       FUNCTION Find(Content:STRING;VAR Link:TBeRoStringTreeLink):BOOLEAN;
     END;

IMPLEMENTATION

FUNCTION HashString(S:STRING):LONGWORD;
VAR Counter,StringLength:INTEGER;
BEGIN
 RESULT:=0;
 StringLength:=LENGTH(S);
 FOR Counter:=1 TO StringLength DO BEGIN
{$IFDEF CPU386}
  ASM
   ROR DWORD PTR RESULT,13
  END;
  INC(RESULT,BYTE(S[Counter]));
{$ELSE}
  RESULT:=((RESULT SHR 13) OR (RESULT SHL (32-13)))+BYTE(S[Counter]);
{$ENDIF}
 END;
END;

FUNCTION CreateStringTreeNode(AChar:CHAR):PBeRoStringTreeNode;
BEGIN
 GETMEM(RESULT,SIZEOF(TBeRoStringTreeNode));
 RESULT^.TheChar:=AChar;
 RESULT^.Link:=0;
 RESULT^.LinkExist:=FALSE;
 RESULT^.Prevoius:=NIL;
 RESULT^.Next:=NIL;
 RESULT^.Up:=NIL;
 RESULT^.Down:=NIL;
END;

PROCEDURE DestroyStringTreeNode(Node:PBeRoStringTreeNode);
BEGIN
 IF NOT ASSIGNED(Node) THEN EXIT;
 DestroyStringTreeNode(Node^.Next);
 DestroyStringTreeNode(Node^.Down);
 FREEMEM(Node);
END;

CONSTRUCTOR TBeRoStringTree.Create;
BEGIN
 INHERITED Create;
 Root:=NIL;
 Clear;
END;

DESTRUCTOR TBeRoStringTree.Destroy;
BEGIN
 Clear;
 INHERITED Destroy;
END;

PROCEDURE TBeRoStringTree.Clear;
VAR Counter:INTEGER;
BEGIN
 DestroyStringTreeNode(Root);
 Root:=NIL;
 FILLCHAR(Hashes,SIZEOF(TBeRoStringHashes),#0);
 FILLCHAR(HashNodes,SIZEOF(TBeRoStringHashNodes),#0);
 FOR Counter:=0 TO MaxStringHashes-1 DO BEGIN
  HashStrings[Counter]:='';
 END;
END;

PROCEDURE TBeRoStringTree.Dump;
VAR Ident:INTEGER;
 PROCEDURE DumpNode(Node:PBeRoStringTreeNode);
 VAR SubNode:PBeRoStringTreeNode;
     IdentCounter,IdentOld:INTEGER;
 BEGIN
  FOR IdentCounter:=1 TO Ident DO WRITE(' ');
  WRITE(Node^.TheChar);
  IdentOld:=Ident;
  SubNode:=Node^.Next;
  WHILE ASSIGNED(SubNode) DO BEGIN
   WRITE(SubNode.TheChar);
   IF NOT ASSIGNED(SubNode^.Next) THEN BREAK;
   INC(Ident);
   SubNode:=SubNode^.Next;
  END;
  WRITELN;
  INC(Ident);
  WHILE ASSIGNED(SubNode) AND (SubNode<>Node) DO BEGIN
   IF ASSIGNED(SubNode^.Down) THEN DumpNode(SubNode^.Down);
   SubNode:=SubNode^.Prevoius;
   DEC(Ident);
  END;
  Ident:=IdentOld;
  IF ASSIGNED(Node^.Down) THEN DumpNode(Node^.Down);
 END;
BEGIN
 Ident:=0;
 DumpNode(Root);
END;

FUNCTION TBeRoStringTree.Add(Content:STRING;Link:TBeRoStringTreeLink;Replace:BOOLEAN=FALSE):BOOLEAN;
VAR StringLength,Position,PositionCounter:INTEGER;
    NewNode,LastNode,Node:PBeRoStringTreeNode;
    StringChar,NodeChar:CHAR;
    Hash,HashToCompare,HashCounter:LONGWORD;
BEGIN
 RESULT:=FALSE;
 Hash:=0;
 StringLength:=LENGTH(Content);
 IF StringLength>0 THEN BEGIN
  IF Hashing THEN BEGIN
   Hash:=HashString(Content);
   FOR HashCounter:=0 TO MaxStringHashes-1 DO BEGIN
    HashToCompare:=Hashes[HashCounter];
    IF HashToCompare<>0 THEN BEGIN
     IF HashToCompare=Hash THEN BEGIN
      IF HashStrings[HashCounter]=Content THEN BEGIN
       IF ASSIGNED(HashNodes[HashCounter]) THEN BEGIN
        LastNode:=HashNodes[HashCounter];
        IF Replace OR NOT LastNode^.LinkExist THEN BEGIN
         LastNode^.Link:=Link;
         RESULT:=TRUE;
        END;
        EXIT;
       END;
      END;
     END;
    END ELSE BEGIN
     BREAK;
    END;
   END;
  END;
  LastNode:=NIL;
  Node:=Root;
  FOR Position:=1 TO StringLength DO BEGIN
   StringChar:=Content[Position];
   IF ASSIGNED(Node) THEN BEGIN
    NodeChar:=Node^.TheChar;
    IF NodeChar=StringChar THEN BEGIN
     LastNode:=Node;
     Node:=Node^.Next;
   END ELSE BEGIN
     WHILE (NodeChar<StringChar) AND ASSIGNED(Node^.Down) DO BEGIN
      Node:=Node^.Down;
      NodeChar:=Node^.TheChar;
     END;
     IF NodeChar=StringChar THEN BEGIN
      LastNode:=Node;
      Node:=Node^.Next;
     END ELSE BEGIN
      NewNode:=CreateStringTreeNode(StringChar);
      IF NodeChar<StringChar THEN BEGIN
       NewNode^.Down:=Node^.Down;
       NewNode^.Up:=Node;
       IF ASSIGNED(NewNode^.Down) THEN BEGIN
        NewNode^.Down^.Up:=NewNode;
       END;
       NewNode^.Prevoius:=Node^.Prevoius;
       Node^.Down:=NewNode;
      END ELSE IF NodeChar>StringChar THEN BEGIN
       NewNode^.Down:=Node;
       NewNode^.Up:=Node^.Up;
       IF ASSIGNED(NewNode^.Up) THEN BEGIN
        NewNode^.Up^.Down:=NewNode;
       END;
       NewNode^.Prevoius:=Node^.Prevoius;
       IF NOT ASSIGNED(NewNode^.Up) THEN BEGIN
        IF ASSIGNED(NewNode^.Prevoius) THEN BEGIN
         NewNode^.Prevoius^.Next:=NewNode;
        END ELSE BEGIN
         Root:=NewNode;
        END;
       END;
       Node^.Up:=NewNode;
      END;
      LastNode:=NewNode;
      Node:=LastNode^.Next;
     END;
    END;
   END ELSE BEGIN
    FOR PositionCounter:=Position TO StringLength DO BEGIN
     NewNode:=CreateStringTreeNode(Content[PositionCounter]);
     IF ASSIGNED(LastNode) THEN BEGIN
      NewNode^.Prevoius:=LastNode;
      LastNode^.Next:=NewNode;
      LastNode:=LastNode^.Next;
     END ELSE BEGIN
      IF NOT ASSIGNED(Root) THEN BEGIN
       Root:=NewNode;
       LastNode:=Root;
      END;
     END;
    END;
    BREAK;
   END;
  END;
  IF ASSIGNED(LastNode) THEN BEGIN
   IF Replace OR NOT LastNode^.LinkExist THEN BEGIN
    IF Hashing THEN BEGIN
     FOR HashCounter:=0 TO MaxStringHashes-2 DO BEGIN
      Hashes[HashCounter+1]:=Hashes[HashCounter];
      HashStrings[HashCounter+1]:=HashStrings[HashCounter];
      HashNodes[HashCounter+1]:=HashNodes[HashCounter];
     END;
     Hashes[0]:=Hash;
     HashStrings[0]:=Content;
     HashNodes[0]:=LastNode;
    END;
    LastNode^.Link:=Link;
    LastNode^.LinkExist:=TRUE;
    RESULT:=TRUE;
   END;
  END;
 END;
END;

FUNCTION TBeRoStringTree.Delete(Content:STRING):BOOLEAN;
VAR StringLength,Position:INTEGER;
    Node:PBeRoStringTreeNode;
    StringChar,NodeChar:CHAR;
    Hash,HashToCompare,HashCounter:LONGWORD;
BEGIN
 RESULT:=FALSE;
 Hash:=0;
 StringLength:=LENGTH(Content);
 IF StringLength>0 THEN BEGIN
  IF Hashing THEN BEGIN
   Hash:=HashString(Content);
   FOR HashCounter:=0 TO MaxStringHashes-1 DO BEGIN
    HashToCompare:=Hashes[HashCounter];
    IF HashToCompare<>0 THEN BEGIN
     IF HashToCompare=Hash THEN BEGIN
      IF HashStrings[HashCounter]=Content THEN BEGIN
       IF ASSIGNED(HashNodes[HashCounter]) THEN BEGIN
        HashNodes[HashCounter]^.LinkExist:=FALSE;
        RESULT:=TRUE;
        EXIT;
       END;
      END;
     END;
    END ELSE BEGIN
     BREAK;
    END;
   END;
  END;
  Node:=Root;
  FOR Position:=1 TO StringLength DO BEGIN
   StringChar:=Content[Position];
   IF ASSIGNED(Node) THEN BEGIN
    NodeChar:=Node^.TheChar;
    WHILE (NodeChar<>StringChar) AND ASSIGNED(Node^.Down) DO BEGIN
     Node:=Node^.Down;
     NodeChar:=Node^.TheChar;
    END;
    IF NodeChar=StringChar THEN BEGIN
     IF (Position=StringLength) AND Node^.LinkExist THEN BEGIN
      IF Hashing THEN BEGIN
       FOR HashCounter:=0 TO MaxStringHashes-2 DO BEGIN
        Hashes[HashCounter+1]:=Hashes[HashCounter];
        HashStrings[HashCounter+1]:=HashStrings[HashCounter];
        HashNodes[HashCounter+1]:=HashNodes[HashCounter];
       END;
       Hashes[0]:=Hash;
       HashStrings[0]:=Content;
       HashNodes[0]:=Node;
      END;
      Node^.LinkExist:=FALSE;
      RESULT:=TRUE;
      BREAK;
     END;
     Node:=Node^.Next;
    END ELSE BEGIN
     BREAK;
    END;
   END ELSE BEGIN
    BREAK;
   END;
  END;
 END;
END;

FUNCTION TBeRoStringTree.Find(Content:STRING;VAR Link:TBeRoStringTreeLink):BOOLEAN;
VAR StringLength,Position:INTEGER;
    Node:PBeRoStringTreeNode;
    StringChar,NodeChar:CHAR;
    Hash,HashToCompare,HashCounter:LONGWORD;
BEGIN
 RESULT:=FALSE;
 Hash:=0;
 StringLength:=LENGTH(Content);
 IF StringLength>0 THEN BEGIN
  IF Hashing THEN BEGIN
   Hash:=HashString(Content);
   FOR HashCounter:=0 TO MaxStringHashes-1 DO BEGIN
    HashToCompare:=Hashes[HashCounter];
    IF HashToCompare<>0 THEN BEGIN
     IF HashToCompare=Hash THEN BEGIN
      IF HashStrings[HashCounter]=Content THEN BEGIN
       IF ASSIGNED(HashNodes[HashCounter]) THEN BEGIN
        Link:=HashNodes[HashCounter]^.Link;
        RESULT:=TRUE;
        EXIT;
       END;
      END;
     END;
    END ELSE BEGIN
     BREAK;
    END;
   END;
  END;
  Node:=Root;
  FOR Position:=1 TO StringLength DO BEGIN
   StringChar:=Content[Position];
   IF ASSIGNED(Node) THEN BEGIN
    NodeChar:=Node^.TheChar;
    WHILE (NodeChar<>StringChar) AND ASSIGNED(Node^.Down) DO BEGIN
     Node:=Node^.Down;
     NodeChar:=Node^.TheChar;
    END;
    IF NodeChar=StringChar THEN BEGIN
     IF (Position=StringLength) AND Node^.LinkExist THEN BEGIN
      IF Hashing THEN BEGIN
       FOR HashCounter:=0 TO MaxStringHashes-2 DO BEGIN
        Hashes[HashCounter+1]:=Hashes[HashCounter];
        HashStrings[HashCounter+1]:=HashStrings[HashCounter];
        HashNodes[HashCounter+1]:=HashNodes[HashCounter];
       END;
       Hashes[0]:=Hash;
       HashStrings[0]:=Content;
       HashNodes[0]:=Node;
      END;
      Link:=Node^.Link;
      RESULT:=TRUE;
      EXIT;
     END;
     Node:=Node^.Next;
    END ELSE BEGIN
     BREAK;
    END;
   END ELSE BEGIN
    BREAK;
   END;
  END;
 END;
END;

END.
