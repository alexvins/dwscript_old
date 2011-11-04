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

{$DEFINE USE_HELPER} //workaround for CodeTools bug

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

uses Windows, Classes, SysUtils, Variants, typinfo
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

   {$IFDEF FPC}

   { EdwsException }

   EdwsException = class(Exception)
   private
      function GetMessage: UnicodeString;
      procedure SetMessage(AValue: UnicodeString);
   public
     constructor Create(const msg : UnicodeString);
     constructor CreateFmt(const msg : UnicodeString; const args : array of const);
     property Message: UnicodeString read GetMessage write SetMessage;
   end;
   {$ELSE}
   EdwsException = Exception;
   {$ENDIF}

   {$IFDEF FPC}
   TDwsCollectionItem = class;
   TdwsCollectionItemClass = class of TDwsCollectionItem;

   { TDwsOwnedCollection }

   TDwsOwnedCollection = class (TOwnedCollection)
   private
      function GetItems(Index: Integer): TDwsCollectionItem;
      procedure SetItems(Index: Integer; AValue: TDwsCollectionItem);
   public
      Constructor Create(AOwner: TPersistent;AItemClass: TdwsCollectionItemClass);
      property Items[Index: Integer]: TDwsCollectionItem read GetItems write SetItems;
   end;



   { TDwsCollectionItem }

   TDwsCollectionItem = class (TCollectionItem)
   private
      FDisplayName: UnicodeString;
      function GetCollection: TDwsOwnedCollection;
      procedure SetCollection(AValue: TDwsOwnedCollection);reintroduce;
   protected
      function GetDisplayName: UnicodeString; reintroduce; virtual;
      procedure SetDisplayName(AValue: UnicodeString);reintroduce; virtual;
   public
      function GetNamePath: UnicodeString; reintroduce; virtual;
      property Collection: TDwsOwnedCollection read GetCollection write SetCollection;
      property DisplayName:UnicodeString read GetDisplayName write SetDisplayName;
   end;

   { TStringsUnicodeHelper }

   TStringsUnicodeHelper = class {$IFDEF USE_HELPER} helper for TStrings {$ENDIF}
   private
      function GetStrings(Index: Integer): UnicodeString;
      procedure SetStrings(Index: Integer; AValue: UnicodeString);
   public
      property Strings[Index: Integer]: UnicodeString read GetStrings write SetStrings; default;
      function AddObject(const S: UnicodeString; AObject: TObject): Integer; overload;
      function Add(const S: UnicodeString): Integer; overload;
      function IndexOf(const S: UnicodeString): Integer; overload;

   end;

   TStringListUnicodeHelper = class {$IFDEF USE_HELPER} helper (TStringsUnicodeHelper) for TStringList  {$ENDIF}

   end;

   { TExceptionUnicodeHepler }

   TExceptionUnicodeHepler = class {$IFDEF USE_HELPER} helper for Exception  {$ENDIF}
   private
      function GetMessage: UnicodeString;
      procedure SetMessage(AValue: UnicodeString);
   public
     property Message: UnicodeString read GetMessage write SetMessage;
   end;

   {$ELSE}
   { TDwsCollectionItem }

   TDwsCollectionItem = TCollectionItem;
   TDwsOwnedCollection = TOwnedCollection;
   {$ENDIF}



function GetSystemMilliseconds : Cardinal;
function UTCDateTime : TDateTime;

function AnsiCompareText(const S1, S2 : UnicodeString) : Integer;
function AnsiCompareStr(const S1, S2 : UnicodeString) : Integer;
function UnicodeComparePChars(p1 : PWideChar; n1 : Integer; p2 : PWideChar; n2 : Integer) : Integer;

function dwsSameText(const s1,s2: UnicodeString): boolean;
function dwsCompareText(const S1, S2 : UnicodeString) : Integer;
function dwsCompareStr(const S1, S2 : UnicodeString) : Integer;



function TextToFloatU(Buffer: PWideChar; Out Value; ValueType: TFloatValue; Const FormatSettings: TFormatSettings): Boolean;

{$IFDEF FPC}
   procedure VarCopy(var ADest: Variant; const ASource: Variant);
{$ENDIF}

function dwsVarToStr(const V: Variant): UnicodeString;
function dwsVarToStrDef(const V: Variant; const ADefault: UnicodeString): UnicodeString;

function VarDataToUniStr(vardata: PVarData): UnicodeString; inline;
procedure UniStrToVarData(vardata: PVarData; AValue: UnicodeString); inline;

function GetExceptObject: TObject;

function GetParentTypeInfo(info: TRTTIInfo):TRTTIInfo;

function dwsStringOfChar(c: WideChar; i: SizeInt):UnicodeString;

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
   {$IFDEF FPC}
   Result:=SysUtils.UnicodeCompareText(S1, S2);
   {$ELSE}
   Result:=SysUtils.AnsiCompareText(S1, S2);
   {$ENDIF}
end;

// AnsiCompareStr
//
function AnsiCompareStr(const S1, S2: UnicodeString) : Integer;
begin
   {$IFDEF FPC}
   Result:=SysUtils.UnicodeCompareStr(S1, S2);
   {$ELSE}
   Result:=SysUtils.AnsiCompareStr(S1, S2);
   {$ENDIF}
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

function dwsSameText(const s1, s2: UnicodeString): boolean;
begin
   {$IFDEF FPC}
   Result := UnicodeSameText(s1,s2);
   {$ELSE}
   Result := SameText(s1,s2);
   {$ENDIF}
end;

function dwsCompareText(const S1, S2: UnicodeString): Integer;
begin
   {$IFDEF FPC}
   Result := UnicodeCompareText(s1,s2);
   {$ELSE}
   Result := CompareText(s1,s2);
   {$ENDIF}
end;

function dwsCompareStr(const S1, S2: UnicodeString): Integer;
begin
   {$IFDEF FPC}
   Result := UnicodeCompareStr(s1,s2);
   {$ELSE}
   Result := CompareStr(s1,s2);
   {$ENDIF}
end;

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

function dwsVarToStr(const V: Variant): UnicodeString;
begin
   {$IFDEF FPC}
   Result := VarToUnicodeStr(V);
   {$ELSE}
   Result := VarToStr(V);
   {$ENDIF}
end;

function dwsVarToStrDef(const V: Variant; const ADefault: UnicodeString
   ): UnicodeString;
begin
   {$IFDEF FPC}
   Result := VarToUnicodeStrDef(V, ADefault);
   {$ELSE}
   Result := VarToStrDef(V, ADefault);
   {$ENDIF}
end;

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

function dwsStringOfChar(c: WideChar; i: SizeInt): UnicodeString;
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

{ TExceptionUnicodeHepler }

function TExceptionUnicodeHepler.GetMessage: UnicodeString;
begin
   Result := UTF8Decode(inherited message);
end;

procedure TExceptionUnicodeHepler.SetMessage(AValue: UnicodeString);
begin
  inherited message := UTF8Encode(AValue);
end;

{ TStringsUnicodeHelper }

function TStringsUnicodeHelper.Add(const S: UnicodeString): Integer;
begin
   Result := Add(UTF8Encode(s));
end;

function TStringsUnicodeHelper.AddObject(const S: UnicodeString;
   AObject: TObject): Integer;
begin
   Result := AddObject(UTF8Encode(s), AObject);
end;

function TStringsUnicodeHelper.GetStrings(Index: Integer): UnicodeString;
begin
   Result := UTF8Decode(Get(Index));
end;

function TStringsUnicodeHelper.IndexOf(const S: UnicodeString): Integer;
begin
   Result := IndexOf(UTF8Encode(s));
end;

procedure TStringsUnicodeHelper.SetStrings(Index: Integer; AValue: UnicodeString
   );
begin
  Put(Index, UTF8Encode(AValue));
end;


{$IFDEF FPC}

constructor TDwsOwnedCollection.Create(AOwner: TPersistent;
   AItemClass: TdwsCollectionItemClass);
begin
   inherited Create(AOwner, AItemClass);
end;

function TDwsOwnedCollection.GetItems(Index: Integer): TDwsCollectionItem;
begin
  Result := TDwsCollectionItem( inherited Items[Index]);
end;

procedure TDwsOwnedCollection.SetItems(Index: Integer; AValue: TDwsCollectionItem);
begin
  inherited Items[Index] := AValue;
end;



{ TDwsCollectionItem }

function TDwsCollectionItem.GetCollection: TDwsOwnedCollection;
begin
   Result := TDwsOwnedCollection(inherited  Collection)
end;

function TDwsCollectionItem.GetDisplayName: UnicodeString;
begin
   Result := FDisplayName;
end;

function TDwsCollectionItem.GetNamePath: UnicodeString;
begin
   Result := inherited GetNamePath;
end;

procedure TDwsCollectionItem.SetCollection(AValue: TDwsOwnedCollection);
begin
   inherited Collection := AValue;
end;

procedure TDwsCollectionItem.SetDisplayName(AValue: UnicodeString);
begin
   FDisplayName := AValue;
   inherited SetDisplayName(UTF8Encode(AValue));
end;
{$ENDIF}

{$IFDEF FPC}
{ EdwsException }

constructor EdwsException.Create(const msg: UnicodeString);
begin
  inherited Create(UTF8Encode(msg));
end;

constructor EdwsException.CreateFmt(const msg: UnicodeString;
   const args: array of const);
begin
   Create(UnicodeFormat(msg,args));
end;

function EdwsException.GetMessage: UnicodeString;
begin
  Result := inherited Message;
end;

procedure EdwsException.SetMessage(AValue: UnicodeString);
begin
  inherited Message := AValue;
end;

{$ENDIF}

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
   Result:=UnicodeString(SysUtils.GetTempFileName(GetTempDir(False),'DWS'));
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
  Result := UnicodeString(SysUtils.GetTempDir(False));
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
