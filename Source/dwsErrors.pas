{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. For other initial contributors, see contributors.txt   }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsErrors;

{$I dws.inc}

interface

uses
   Classes, SysUtils, dwsStrings, dwsUtils;

type

   TdwsMessageList = class;

   // TSourceFile
   //
   TSourceFile = class
      public
         Name : UnicodeString;
         Code : UnicodeString;
   end;

   // TScriptPos
   //
   PScriptPos = ^TScriptPos;
   TScriptPos = packed record
      private
         // 12bits for the column (4096)
         // 20bits for the line (1048576)
         FLineCol : Cardinal;

         function GetLine : Integer; inline;
         procedure SetLine(const aLine : Integer); inline;
         function GetCol : Integer; inline;
         procedure SetCol(const aCol : Integer); inline;

      public
         SourceFile : TSourceFile;

         const cLineMask = $FFFFF;

         constructor Create(aSourceFile : TSourceFile; aLine, aCol : Integer);

         property LineCol : Cardinal read FLineCol write FLineCol;
         property Line : Integer read GetLine write SetLine;
         property Col : Integer read GetCol write SetCol;

         function SamePosAs(const aPos : TScriptPos) : Boolean;
         function IsMainModule : Boolean;
         function IsSourceFile(const name : UnicodeString) : Boolean;
         function Defined : Boolean;

         procedure IncCol; inline;
         procedure NewLine; inline;

         function AsInfo : UnicodeString;
   end;
   TScriptPosArray = array of TScriptPos; // dynamic array that can hold ScriptPos settings (needed for ReadNameList)

   // TdwsMessage
   //
   TdwsMessage = class abstract
      private
         FMsgs: TdwsMessageList;
         FText: UnicodeString;

      public
         constructor Create(Msgs: TdwsMessageList; const Text: UnicodeString);

         function AsInfo: UnicodeString; virtual; abstract;
         property Text : UnicodeString read FText;
   end;

   // Messages without position

   TInfoMessage = class(TdwsMessage)
      function AsInfo: UnicodeString; override;
   end;

   // Messages with position

   // TScriptMessage
   //
   TScriptMessage = class(TdwsMessage)
      private
         FPos : TScriptPos;

      public
         constructor Create(msgs: TdwsMessageList; const text : UnicodeString; const p : TScriptPos); overload;
         function AsInfo : UnicodeString; override;

         property Pos : TScriptPos read FPos write FPos;
   end;

   TScriptMessageClass = class of TScriptMessage;

   THintMessage = class(TScriptMessage)
      function AsInfo: UnicodeString; override;
   end;

   TWarningMessage = class(TScriptMessage)
      function AsInfo: UnicodeString; override;
   end;

   TCompilerErrorMessage = class(TScriptMessage)
      function AsInfo: UnicodeString; override;
   end;

   TSyntaxErrorMessage = class(TScriptMessage)
      function AsInfo: UnicodeString; override;
   end;

   // TdwsMessageList
   //
   TdwsMessageList = class
      private
         FMessageList : TTightList;
         FSourceFiles : TTightList;
         FHasErrors : Boolean;

      protected
         function GetMsg(Index: Integer): TdwsMessage;
         function GetMsgCount: Integer;

      public
         destructor Destroy; override;

         procedure AddInfo(const Text: UnicodeString);
         function LastMessagePos : TScriptPos;

         procedure AddMsg(aMessage : TdwsMessage); virtual;
         procedure AddMsgs(src : TdwsMessageList; lineOffset, colOffset : Integer);
         procedure Clear;

         function AsInfo: UnicodeString;

         property Msgs[index : Integer] : TdwsMessage read GetMsg; default;
         property Count : Integer read GetMsgCount;
         property HasErrors : Boolean read FHasErrors write FHasErrors;
   end;

   // TdwsCompileMessageList
   //
   TdwsCompileMessageList = class (TdwsMessageList)
      private
         FHintsDisabled : Boolean;
         FWarningsDisabled : Boolean;

      public
         procedure AddCompilerInfo(const Text: UnicodeString);

         procedure AddCompilerHint(const Pos: TScriptPos; const Text: UnicodeString); overload;
         procedure AddCompilerHintFmt(const Pos: TScriptPos; const textFormat : UnicodeString; const args: array of const); overload;

         procedure AddCompilerWarning(const Pos: TScriptPos; const Text: UnicodeString);
         procedure AddCompilerWarningFmt(const Pos: TScriptPos; const textFormat : UnicodeString; const args: array of const);

         procedure AddCompilerError(const Pos: TScriptPos; const Text: UnicodeString; messageClass : TScriptMessageClass); overload;
         procedure AddCompilerError(const Pos: TScriptPos; const Text: UnicodeString); overload;
         procedure AddCompilerErrorFmt(const Pos: TScriptPos; const textFormat : UnicodeString; const args: array of const; messageClass : TScriptMessageClass); overload;
         procedure AddCompilerErrorFmt(const Pos: TScriptPos; const textFormat : UnicodeString; const args: array of const); overload;

         procedure AddCompilerStop(const Pos: TScriptPos; const Text: UnicodeString; messageClass : TScriptMessageClass); overload;
         procedure AddCompilerStop(const Pos: TScriptPos; const Text: UnicodeString); overload;
         procedure AddCompilerStopFmt(const Pos: TScriptPos; const textFormat : UnicodeString; const args: array of const; messageClass : TScriptMessageClass); overload;
         procedure AddCompilerStopFmt(const Pos: TScriptPos; const textFormat : UnicodeString; const args: array of const); overload;

         property HintsDisabled : Boolean read FHintsDisabled write FHintsDisabled;
         property WarningsDisabled : Boolean read FWarningsDisabled write FWarningsDisabled;
   end;

   // The script initialization failed because a class needs one or more methods
   // to be implemented.
   EClassIncompleteError = class(Exception)
      private
         FClassSymObj: TObject;   // object that refers to the TClassSymbol

      public
         property ClassSymObj: TObject read FClassSymObj write FClassSymObj;
   end;

   EClassPropertyIncompleteError = class(EClassIncompleteError);

   // The compilation has to be stopped because of an error
   ECompileError = class(Exception)
      private
         FScriptPos : TScriptPos;

      public
         constructor CreatePosFmt(const pos : TScriptPos; const Msg: UnicodeString; const Args: array of const);

         property Pos : TScriptPos read FScriptPos write FScriptPos;
   end;

   EReraise = class(Exception);

const
   cNullPos: TScriptPos = (FLineCol: 0; SourceFile: nil);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TScriptPos ------------------
// ------------------

// Create
//
constructor TScriptPos.Create(aSourceFile : TSourceFile; aLine, aCol : Integer);
begin
   SourceFile:=aSourceFile;
   FLineCol:=(aCol shl 20)+aLine;
end;

// GetLine
//
function TScriptPos.GetLine : Integer;
begin
   Result:=FLineCol and cLineMask;
end;

// SetLine
//
procedure TScriptPos.SetLine(const aLine : Integer);
begin
   FLineCol:=(FLineCol and $FFF00000) or Cardinal(aLine);
end;

// GetCol
//
function TScriptPos.GetCol : Integer;
begin
   Result:=(FLineCol shr 20) and $FFF;
end;

// SetCol
//
procedure TScriptPos.SetCol(const aCol : Integer);
begin
   FLineCol:=(FLineCol and cLineMask) or (Cardinal(aCol) shl 20);
end;

// SamePosAs
//
function TScriptPos.SamePosAs(const aPos : TScriptPos) : Boolean;
begin
   Result:=    (FLineCol=aPos.FLineCol)
           and (SourceFile=aPos.SourceFile);
end;

// IsMainModule
//
function TScriptPos.IsMainModule : Boolean;
begin
   Result:=(SourceFile=nil) or (SourceFile.Name=MSG_MainModule);
end;

// IsSourceFile
//
function TScriptPos.IsSourceFile(const name : UnicodeString) : Boolean;
begin
   Result:=(SourceFile<>nil) and (SourceFile.Name=name);
end;

// Defined
//
function TScriptPos.Defined : Boolean;
begin
   Result:=(SourceFile<>nil) and (FLineCol<>0);
end;

// IncCol
//
procedure TScriptPos.IncCol;
begin
   Inc(FLineCol, $100000);
end;

// NewLine
//
procedure TScriptPos.NewLine;
begin
   FLineCol:=(FLineCol and cLineMask)+$100001;
end;

// AsInfo
//
function TScriptPos.AsInfo : UnicodeString;
begin
   if SourceFile=nil then
      Result:=''
   else begin
      if not IsMainModule then
         Result:=Format(MSG_ScriptPosFile, [SourceFile.Name])
      else Result:='';
      if Col<>cNullPos.Col then begin
         if Result<>'' then
            Result:=', '+Result;
         Result:=Format(MSG_ScriptPosColumn, [Col])+Result;
      end;
      if Line<>cNullPos.Line then begin
         if Result<>'' then
            Result:=', '+Result;
         Result:=Format(MSG_ScriptPosLine, [Line])+Result;
      end;
      if Result<>'' then
         Result:=' ['+Result+']';
   end;
end;

// ------------------
// ------------------ ECompileError ------------------
// ------------------

// CreatePosFmt
//
constructor ECompileError.CreatePosFmt(const pos : TScriptPos; const Msg: UnicodeString; const Args: array of const);
begin
   inherited CreateFmt(msg, args);
   FScriptPos:=pos;
end;

// ------------------
// ------------------ TdwsMessageList ------------------
// ------------------

// Destroy
//
destructor TdwsMessageList.Destroy;
begin
   FMessageList.Clean;
   FSourceFiles.Clean;
   inherited;
end;

// Clear
//
procedure TdwsMessageList.Clear;
begin
   FMessageList.Clean;
   FSourceFiles.Clean;
   FHasErrors:=False;
end;

// GetMsg
//
function TdwsMessageList.GetMsg(Index: Integer): TdwsMessage;
begin
   Result:=TdwsMessage(FMessageList.List[Index]);
end;

// GetMsgCount
//
function TdwsMessageList.GetMsgCount: Integer;
begin
   Result:=FMessageList.Count;
end;

// AddMsg
//
procedure TdwsMessageList.AddMsg(aMessage: TdwsMessage);
begin
   FMessageList.Add(aMessage);
end;

// AddMsgs
//
procedure TdwsMessageList.AddMsgs(src : TdwsMessageList; lineOffset, colOffset : Integer);
var
   i : Integer;
   msg : TdwsMessage;
   srcMsg : TScriptMessage;
   sf : TSourceFile;
begin
   for i:=0 to src.Count-1 do begin
      msg:=src.Msgs[i];
      if msg is TScriptMessage then begin
         srcMsg:=TScriptMessage(msg);
         sf:=TSourceFile.Create;
         sf.Name:=srcMsg.Pos.SourceFile.Name;
         sf.Code:=srcMsg.Pos.SourceFile.Code;
         FSourceFiles.Add(sf);
         srcMsg.FPos.SourceFile:=sf;
         if srcMsg.Pos.Line=1 then
            srcMsg.Pos.Col:=srcMsg.Pos.Col+colOffset;
         srcMsg.Pos.Line:=srcMsg.Pos.Line+lineOffset;
      end;
      AddMsg(msg);
   end;
   src.FMessageList.Clear;
   src.FSourceFiles.Clear;
end;

// AddInfo
//
procedure TdwsMessageList.AddInfo(const Text: UnicodeString);
begin
   AddMsg(TInfoMessage.Create(Self, Text));
end;

// LastMessagePos
//
function TdwsMessageList.LastMessagePos : TScriptPos;
var
   lastMsg : TdwsMessage;
begin
   if Count=0 then
      lastMsg:=nil
   else lastMsg:=Msgs[Count-1];
   if lastMsg is TScriptMessage then
      Result:=TScriptMessage(lastMsg).Pos
   else Result:=cNullPos
end;

// AsInfo
//
function TdwsMessageList.AsInfo: UnicodeString;
var
   i: Integer;
begin
   Result:='';
   for i:=0 to Count-1 do
      Result:=Result+Msgs[i].AsInfo+#13#10
end;

// ------------------
// ------------------ TdwsMessage ------------------
// ------------------

// Create
//
constructor TdwsMessage.Create(Msgs: TdwsMessageList; const Text: UnicodeString);
begin
   FMsgs:=Msgs;
   FText:=Text;
end;

// ------------------
// ------------------ TInfoMessage ------------------
// ------------------

// AsInfo
//
function TInfoMessage.AsInfo: UnicodeString;
begin
   Result:=Format(MSG_Info, [Text]);
end;

// ------------------
// ------------------ TScriptMessage ------------------
// ------------------

// Create
//
constructor TScriptMessage.Create(Msgs: TdwsMessageList; const Text: UnicodeString; const P: TScriptPos);
begin
   inherited Create(Msgs, Text);
   Pos:=P;
end;

// AsInfo
//
function TScriptMessage.AsInfo: UnicodeString;
begin
   Result:=FText+Pos.AsInfo
end;

// ------------------
// ------------------ THintMessage ------------------
// ------------------

// AsInfo
//
function THintMessage.AsInfo: UnicodeString;
begin
   Result:=Format(MSG_Hint, [inherited AsInfo]);
end;

// ------------------
// ------------------ TWarningMessage ------------------
// ------------------

// AsInfo
//
function TWarningMessage.AsInfo: UnicodeString;
begin
   Result:=Format(MSG_Warning, [inherited AsInfo]);
end;

// ------------------
// ------------------ TCompilerErrorMessage ------------------
// ------------------

// AsInfo
//
function TCompilerErrorMessage.AsInfo: UnicodeString;
begin
   Result:=Format(MSG_CompileError, [inherited AsInfo]);
end;

// ------------------
// ------------------ TSyntaxErrorMessage ------------------
// ------------------

// AsInfo
//
function TSyntaxErrorMessage.AsInfo: UnicodeString;
begin
   Result:=Format(MSG_SyntaxError, [inherited AsInfo]);
end;

// ------------------
// ------------------ TdwsCompileMessageList ------------------
// ------------------

// AddCompilerInfo
//
procedure TdwsCompileMessageList.AddCompilerInfo;
begin
   AddMsg(TInfoMessage.Create(Self, Text));
end;

// AddCompilerHint
//
procedure TdwsCompileMessageList.AddCompilerHint(const Pos: TScriptPos; const Text: UnicodeString);
begin
   if not (HintsDisabled or HasErrors) then
      AddMsg(THintMessage.Create(Self, Text, Pos));
end;

// AddCompilerHintFmt
//
procedure TdwsCompileMessageList.AddCompilerHintFmt(const Pos: TScriptPos; const textFormat : UnicodeString; const args: array of const);
begin
   AddCompilerHint(Pos, Format(textFormat, args));
end;

// AddCompilerWarning
//
procedure TdwsCompileMessageList.AddCompilerWarning(const Pos: TScriptPos; const Text: UnicodeString);
begin
   if not WarningsDisabled then
      AddMsg(TWarningMessage.Create(Self, Text, Pos));
end;

// AddCompilerWarningFmt
//
procedure TdwsCompileMessageList.AddCompilerWarningFmt(const Pos: TScriptPos; const textFormat : UnicodeString; const args: array of const);
begin
   AddCompilerWarning(Pos, Format(textFormat, args));
end;

// AddCompilerError
//
procedure TdwsCompileMessageList.AddCompilerError(const Pos: TScriptPos; const Text: UnicodeString; messageClass : TScriptMessageClass);
begin
   AddMsg(messageClass.Create(Self, Text, Pos));
   FHasErrors:=True;
end;

// AddCompilerError
//
procedure TdwsCompileMessageList.AddCompilerError(const Pos: TScriptPos; const Text: UnicodeString);
begin
   AddCompilerError(Pos, Text, TSyntaxErrorMessage);
end;

// AddCompilerErrorFmt
//
procedure TdwsCompileMessageList.AddCompilerErrorFmt(const Pos: TScriptPos;
   const textFormat: UnicodeString; const args: array of const; messageClass : TScriptMessageClass);
begin
   AddCompilerError(Pos, Format(textFormat, args), messageClass);
end;

// AddCompilerErrorFmt
//
procedure TdwsCompileMessageList.AddCompilerErrorFmt(const Pos: TScriptPos;
   const textFormat: UnicodeString; const args: array of const);
begin
   AddCompilerErrorFmt(Pos, textFormat, args, TSyntaxErrorMessage);
end;

// AddCompilerStop
//
procedure TdwsCompileMessageList.AddCompilerStop(const Pos: TScriptPos; const Text: UnicodeString; messageClass : TScriptMessageClass);
begin
   AddCompilerError(Pos, Text, messageClass);
   raise ECompileError.Create(Text);
end;

// AddCompilerStop
//
procedure TdwsCompileMessageList.AddCompilerStop(const Pos: TScriptPos; const Text: UnicodeString);
begin
   AddCompilerStop(Pos, Text, TSyntaxErrorMessage);
end;

// AddCompilerStopFmt
//
procedure TdwsCompileMessageList.AddCompilerStopFmt(const Pos: TScriptPos; const textFormat : UnicodeString;
                                   const args: array of const; messageClass : TScriptMessageClass);
begin
   AddCompilerStop(Pos, Format(textFormat, args), messageClass);
end;

// AddCompilerStopFmt
//
procedure TdwsCompileMessageList.AddCompilerStopFmt(const Pos: TScriptPos; const textFormat : UnicodeString; const args: array of const);
begin
   AddCompilerStop(Pos, Format(textFormat, args), TSyntaxErrorMessage);
end;

end.
