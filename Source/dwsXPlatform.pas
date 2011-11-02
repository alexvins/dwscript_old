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
unit dwsXPlatform;

{$I dws.inc}

//
// This unit should concentrate all non-UI cross-platform aspects,
// cross-Delphi versions, ifdefs and other conditionals
//
// no ifdefs in the main code.

{$WARN SYMBOL_PLATFORM OFF}

{$IFDEF FPC}
   {$DEFINE VER200}
{$ENDIF}

interface

uses Windows, Classes, SysUtils, typinfo
   {$IFNDEF VER200}, IOUtils{$ENDIF}
   ;

const
{$IFDEF FPC}
   cLineTerminator  = System.LineEnding;
{$ELSE}
  {$IFDEF UNIX}
     cLineTerminator  = #10;
  {$ELSE}
     cLineTerminator  = #13#10;
  {$ENDIF}
{$ENDIF}

procedure SetDecimalSeparator(c : WideChar);
function GetDecimalSeparator : WideChar;

procedure CollectFiles(const directory, fileMask : UnicodeString; list : TStrings);

type
   {$IFDEF FPC}
   PNativeInt = ^NativeInt;
   PNativeUInt = ^NativeUInt;
   Int32 = longint;
   {$ELSE}
   {$IF CompilerVersion<22.0}
   // NativeUInt broken in D2009, and PNativeInt is missing in D2010
   // http://qc.embarcadero.com/wc/qcmain.aspx?d=71292
   NativeInt = Integer;
   PNativeInt = ^NativeInt;
   NativeUInt = Cardinal;
   PNativeUInt = ^NativeUInt;
   {$IFEND}
   {$ENDIF}

   {$IFDEF FPC}
   {$IF FPC_FULLVERSION >=20700}
   TBytes = SysUtils.TBytes;
   {$ELSE}
   TBytes = array of Byte;
   {$ENDIF}
   {$IFNDEF FPC_HAS_CPSTRING}
   RawByteString = type AnsiString;
   {$else}
   //RawByteString is defined in System
   {$ENDIF}
   {$ENDIF}

  {$IFDEF FPC}
    TRTTIInfo = PTypeInfo;
  {$ELSE}
    TRTTIInfo = PPTypeInfo;
  {$ENDIF}

   { TPath }

   TPath = class
      class function GetTempFileName : UnicodeString; static;
      class function GetTemporaryFilesPath : UnicodeString; static;
   end;

   TFile = class
      class function ReadAllBytes(const filename : UnicodeString) : TBytes; static;
   end;

   TdwsThread = class (TThread)
      {$IFNDEF FPC}
      {$IFDEF VER200}
      procedure Start;
      {$ENDIF}
      {$ENDIF}
   end;

   {$IFDEF FPC}
   //TODO: write effective implementation

   { TStringBuilder }

   TStringBuilder = class
   private
      FString:UnicodeString;
   public
      function ToString: UnicodeString; reintroduce;
      procedure Append(s: UnicodeString);
      procedure AppendLine;
   end;
   {$ENDIF}

function GetSystemMilliseconds : Cardinal;
function UTCDateTime : TDateTime;

function AnsiCompareText(const S1, S2 : UnicodeString) : Integer;
function AnsiCompareStr(const S1, S2 : UnicodeString) : Integer;
function UnicodeComparePChars(p1 : PWideChar; n1 : Integer; p2 : PWideChar; n2 : Integer) : Integer;

function TextToFloatU(Buffer: PWideChar; Out Value; ValueType: TFloatValue; Const FormatSettings: TFormatSettings): Boolean;

{$IFDEF FPC}
   procedure VarCopy(var ADest: Variant; const ASource: Variant);
{$ENDIF}

function VarDataToUniStr(vardata: PVarData): UnicodeString; inline;
procedure UniStrToVarData(vardata: PVarData; AValue: UnicodeString); inline;

function GetExceptObject: TObject;

function GetParentTypeInfo(info: TRTTIInfo):TRTTIInfo;

function UnicodeStringOfChar(c: WideChar; i: SizeInt):UnicodeString;

Function dwsFormat (Const Fmt : UnicodeString; const Args : Array of const) : UnicodeString; overload; {$IFNDEF FPC} inline; {$ENDIF}
Function dwsFormat (Const Fmt : UnicodeString; const Args : Array of const; Const FormatSettings: TFormatSettings) : UnicodeString; overload;{$IFNDEF FPC} inline; {$ENDIF}


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// GetSystemMilliseconds
//
function GetSystemMilliseconds : Cardinal;
begin
   Result:=GetTickCount;
end;

// UTCDateTime
//
function UTCDateTime : TDateTime;
var
   systemTime : TSystemTime;
begin
   GetSystemTime(systemTime); //todo: crossplatform solution
   {$IFDEF FPC}
   Result := SystemTimeToDateTime(systemTime);
   {$ELSE}
   with systemTime do
      Result:= EncodeDate(wYear, wMonth, wDay)
              +EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
   {$ENDIF}

end;

// AnsiCompareText
//
function AnsiCompareText(const S1, S2: UnicodeString) : Integer;
begin
   Result:=SysUtils.AnsiCompareText(S1, S2);
end;

// AnsiCompareStr
//
function AnsiCompareStr(const S1, S2: UnicodeString) : Integer;
begin
   Result:=SysUtils.AnsiCompareStr(S1, S2);
end;

// UnicodeComparePChars
//
function UnicodeComparePChars(p1 : PWideChar; n1 : Integer; p2 : PWideChar; n2 : Integer) : Integer;
const
   CSTR_EQUAL = 2;
begin
   Result:=CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, p1, n1, p2, n2)-CSTR_EQUAL;
end;

{$IFDEF FPC}

function TextToFloatU(Buffer: PWideChar; out Value; ValueType: TFloatValue;
  const FormatSettings: TFormatSettings): Boolean;
var
  temp: AnsiString;
begin
{$IFDEF FPC}
   WideCharToStrVar(Buffer, temp);
   Result := TextToFloat(@temp[1], Value, ValueType, FormatSettings);
{$ELSE}
   Result := TextToFloat(Buffer, Value, ValueType, FormatSettings);
{$ENDIF}
end;

procedure VarCopy(var ADest: Variant; const ASource: Variant);
begin
  ADest := ASource;
end;
{$ENDIF}


function VarDataToUniStr(vardata: PVarData): UnicodeString;
begin
{$IFDEF FPC}
   Result := UnicodeString(varData.vstring);
{$ELSE}
   Result := UnicodeString(varData.VUString);
{$ENDIF}
end;

procedure UniStrToVarData(vardata: PVarData; AValue: UnicodeString);
begin
  {$IFDEF FPC}
     UnicodeString(varData.vstring):=AValue;
  {$ELSE}
     UnicodeString(varData.VUString):=AValue;
  {$ENDIF}
end;

function GetExceptObject: TObject;
begin
  {$IFDEF FPC}
  Result := RaiseList.FObject;
  {$ELSE}
  Result := System.ExceptObject;
  {$ENDIF}
end;

function GetParentTypeInfo(info: TRTTIInfo): TRTTIInfo;
begin
  {$IFDEF FPC}
  Result :=  GetTypeData(info)^.ParentInfo;
  {$ELSE}
  Result :=  GetTypeData(info^)^.ParentInfo;
  {$ENDIF}
end;

function UnicodeStringOfChar(c: WideChar; i: SizeInt): UnicodeString;
begin
  SetLength(Result,i);
  {$IFDEF FPC}
  {$PUSH}{$R-}
  {$ENDIF}
  FillWord(Result[1],i,Word(c));
  {$IFDEF FPC}
  {$POP}
  {$ENDIF}
end;

function dwsFormat(const Fmt: UnicodeString;
  const Args: array of const): UnicodeString;
begin
  {$IFDEF FPC}
  Result := UnicodeFormat(Fmt, Args);
  {$ELSE}
  Result := Format(Fmt, Args);
  {$ENDIF}
end;

function dwsFormat(const Fmt: UnicodeString; const Args: array of const;
  const FormatSettings: TFormatSettings): UnicodeString;
begin
  {$IFDEF FPC}
  Result := UnicodeFormat(Fmt, Args, FormatSettings);
  {$ELSE}
  Result := Format(Fmt, Args, FormatSettings);
  {$ENDIF}
end;

// SetDecimalSeparator
//
procedure SetDecimalSeparator(c : WideChar);
begin
   {$IFDEF FPC}
      FormatSettings.DecimalSeparator:=c;
   {$ELSE}
      {$IF CompilerVersion >= 22.0}
      FormatSettings.DecimalSeparator:=c;
      {$ELSE}
      DecimalSeparator:=c;
      {$IFEND}
   {$ENDIF}
end;

// GetDecimalSeparator
//
function GetDecimalSeparator : WideChar;
begin
   {$IFDEF FPC}
      Result:=FormatSettings.DecimalSeparator;
   {$ELSE}
      {$IF CompilerVersion >= 22.0}
      Result:=FormatSettings.DecimalSeparator;
      {$ELSE}
      Result:=DecimalSeparator;
      {$IFEND}
   {$ENDIF}
end;

// CollectFiles
//
procedure CollectFiles(const directory, fileMask : UnicodeString; list : TStrings);
var
   searchRec : TSearchRec;
   found : Integer;
begin
   found:=FindFirst(directory+fileMask, faArchive or faReadOnly or faHidden, searchRec);
   while found=0 do begin
      if (searchRec.Attr and faDirectory)=0 then begin
         list.Add(directory+searchRec.Name);
      end;
      found:=FindNext(searchRec);
   end;
   FindClose(searchRec);
end;

{$IFDEF FPC}
{ TStringBuilder }

procedure TStringBuilder.Append(s: UnicodeString);
begin
  FString += s;
end;

procedure TStringBuilder.AppendLine;
begin
  FString += cLineTerminator;
end;

function TStringBuilder.ToString: UnicodeString;
begin
  Result := FString;
end;
{$ENDIF}

// ------------------
// ------------------ TPath ------------------
// ------------------

// GetTempFileName
//
class function TPath.GetTempFileName : UnicodeString;
{$IFDEF FPC}
begin
   Result:=SysUtils.GetTempFileName(GetTempDir(False),'DWS');
{$ELSE}
{$IFDEF VER200} // Delphi 2009
var
   tempPath, tempFileName : array [0..MAX_PATH] of WideChar; // Buf sizes are MAX_PATH+1
begin
   if Windows.GetTempPath(MAX_PATH, @tempPath[0])=0 then
      tempPath:='.'; // Current directory
   if Windows.GetTempFileName(@tempPath[0], 'DWS', 0, tempFileName)=0 then
      RaiseLastOSError; // should never happen
   Result:=tempFileName;
{$ELSE}
begin
   Result:=IOUTils.TPath.GetTempFileName;
{$ENDIF}
{$ENDIF}
end;

class function TPath.GetTemporaryFilesPath: UnicodeString;
begin
  {$IFDEF FPC}
  Result := SysUtils.GetTempDir(False);
  {$ELSE}
  {$ERROR NOT IMPLEMENTED}
  {$ENDIF}
end;

// ------------------
// ------------------ TFile ------------------
// ------------------

// ReadAllBytes
//
class function TFile.ReadAllBytes(const filename : UnicodeString) : TBytes;
{$IFDEF VER200} // Delphi 2009
var
   fileStream : TFileStream;
   n : Integer;
begin
   fileStream:=TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
   try
      n:=fileStream.Size;
      SetLength(Result, n);
      if n>0 then
         fileStream.ReadBuffer(Result[0], n);
   finally
      fileStream.Free;
   end;
{$ELSE}
begin
   Result:=IOUTils.TFile.ReadAllBytes(filename);
{$ENDIF}
end;

// ------------------
// ------------------ TdwsThread ------------------
// ------------------

{$IFNDEF FPC}
{$IFDEF VER200}

// Start
//
procedure TdwsThread.Start;
begin
   Resume;
end;

{$ENDIF}
{$ENDIF}

end.
