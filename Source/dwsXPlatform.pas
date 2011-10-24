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

uses Windows, Classes, SysUtils
   {$IFNDEF VER200}, IOUtils{$ENDIF}
   ;

const
{$IFDEF UNIX}
   cLineTerminator  = #10;
{$ELSE}
   cLineTerminator  = #13#10;
{$ENDIF}

procedure SetDecimalSeparator(c : Char);
function GetDecimalSeparator : Char;

procedure CollectFiles(const directory, fileMask : UnicodeString; list : TStrings);

type
   {$IFNDEF FPC}
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
   TBytes = array of Byte;

   RawByteString = String;
   {$ENDIF}

   TPath = class
      class function GetTempFileName : UnicodeString; static;
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

function GetSystemMilliseconds : Cardinal;
function UTCDateTime : TDateTime;

function AnsiCompareText(const S1, S2 : UnicodeString) : Integer;
function AnsiCompareStr(const S1, S2 : UnicodeString) : Integer;
function UnicodeComparePChars(p1 : PChar; n1 : Integer; p2 : PChar; n2 : Integer) : Integer;

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
   GetSystemTime(systemTime);
   with systemTime do
      Result:= EncodeDate(wYear, wMonth, wDay)
              +EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
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
function UnicodeComparePChars(p1 : PChar; n1 : Integer; p2 : PChar; n2 : Integer) : Integer;
const
   CSTR_EQUAL = 2;
begin
   Result:=CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, p1, n1, p2, n2)-CSTR_EQUAL;
end;

// SetDecimalSeparator
//
procedure SetDecimalSeparator(c : Char);
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
function GetDecimalSeparator : Char;
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

// ------------------
// ------------------ TPath ------------------
// ------------------

// GetTempFileName
//
class function TPath.GetTempFileName : UnicodeString;
{$IFDEF VER200} // Delphi 2009
var
   tempPath, tempFileName : array [0..MAX_PATH] of Char; // Buf sizes are MAX_PATH+1
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
