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
unit dwsFileSystem;

{$I dws.inc}

interface

uses Classes, SysUtils, dwsUtils;

type

   // TdwsFileOpenMode
   //
   TdwsFileOpenMode = (
      fomReadOnly,   // opens file for read-only, fails if doesn't exist
      fomReadWrite,  // opens file for read-write, creates a new one if doesn't exist
      fomCreate      // opens file for read-write, always empty
      );

   EdwsFileSystemException = class (Exception)
   end;

   EdwsFSInvalidFileName = class (EdwsFileSystemException)
   end;

   // IdwsFileSystem
   //
   IdwsFileSystem = interface
      ['{D49F19A9-46C6-43E1-AF29-BDB8602A098C}']
      function FileExists(const fileName : UnicodeString) : Boolean;
      function OpenFileStream(const fileName : UnicodeString; const mode : TdwsFileOpenMode) : TStream;
   end;

   // TdwsBaseFileSystem
   //
   {: Minimal virtualized filesystem interface. }
   TdwsBaseFileSystem = class abstract (TInterfacedObject, IdwsFileSystem)
      public
         constructor Create; virtual;

         function FileExists(const fileName : UnicodeString) : Boolean; virtual; abstract;
         function OpenFileStream(const fileName : UnicodeString; const mode : TdwsFileOpenMode) : TStream; virtual; abstract;
   end;

   // TdwsNullFileSystem
   //
   {: Gives access to nothing. }
   TdwsNullFileSystem = class (TdwsBaseFileSystem)
      public
         function FileExists(const fileName : UnicodeString) : Boolean; override;
         function OpenFileStream(const fileName : UnicodeString; const mode : TdwsFileOpenMode) : TStream; override;
   end;

   // TdwsOSFileSystem
   //
   {: Gives access to the whole OS FileSystem. }
   TdwsOSFileSystem = class (TdwsBaseFileSystem)
      public
         function ValidateFileName(const fileName : UnicodeString) : UnicodeString; virtual;

         function FileExists(const fileName : UnicodeString) : Boolean; override;
         function OpenFileStream(const fileName : UnicodeString; const mode : TdwsFileOpenMode) : TStream; override;
   end;

   // TdwsRestrictedOSFileSystem
   //
   {: Gives access to only specified directories and their subdirectories. }
   TdwsRestrictedOSFileSystem = class (TdwsOSFileSystem)
      private
         FPaths : TStrings;
         FPathsPrepared : Boolean;

      protected
         procedure SetPaths(const val : TStrings);
         procedure PathsChanged(Sender : TObject);
         procedure PreparePaths;

      public
         constructor Create; override;
         destructor Destroy; override;

         function ValidateFileName(const fileName : UnicodeString) : UnicodeString; override;

         property Paths : TStrings read FPaths write SetPaths;
   end;

   // TdwsCustomFileSystem
   //
   TdwsCustomFileSystem = class abstract (TComponent)
      public
         function AllocateFileSystem : IdwsFileSystem; virtual; abstract;
   end;

   // TdwsNoFileSystem
   //
   TdwsNoFileSystem = class abstract (TdwsCustomFileSystem)
      public
         function AllocateFileSystem : IdwsFileSystem; override;
   end;

   // TdwsRestrictedFileSystem
   //
   TdwsRestrictedFileSystem = class abstract (TdwsCustomFileSystem)
      private
         FPaths : TStrings;

      protected
         procedure SetPaths(const val : TStrings);

      public
         constructor Create(Owner : TComponent); override;
         destructor Destroy; override;

         function AllocateFileSystem : IdwsFileSystem; override;

      published
         property Paths : TStrings read FPaths write SetPaths;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsBaseFileSystem ------------------
// ------------------

// Create
//
constructor TdwsBaseFileSystem.Create;
begin
   inherited Create;
end;

// ------------------
// ------------------ TdwsNullFileSystem ------------------
// ------------------

// FileExists
//
function TdwsNullFileSystem.FileExists(const fileName : UnicodeString) : Boolean;
begin
   Result:=False;
end;

// OpenFileStream
//
function TdwsNullFileSystem.OpenFileStream(const fileName : UnicodeString; const mode : TdwsFileOpenMode) : TStream;
begin
   Result:=nil;
end;

// ------------------
// ------------------ TdwsOSFileSystem ------------------
// ------------------

// ValidateFileName
//
function TdwsOSFileSystem.ValidateFileName(const fileName : UnicodeString) : UnicodeString;
begin
   // accept all
   Result:=fileName;
end;

// FileExists
//
function TdwsOSFileSystem.FileExists(const fileName : UnicodeString) : Boolean;
var
   validFileName : UnicodeString;
begin
   validFileName:=ValidateFileName(fileName);
   Result:=SysUtils.FileExists(validFileName);
end;

// OpenFileStream
//
function TdwsOSFileSystem.OpenFileStream(const fileName : UnicodeString; const mode : TdwsFileOpenMode) : TStream;
var
   validFileName : UnicodeString;
begin
   validFileName:=ValidateFileName(fileName);
   case mode of
      fomReadOnly :
         Result:=TFileStream.Create(validFileName, fmOpenRead or fmShareDenyWrite);
      fomReadWrite :
         Result:=TFileStream.Create(validFileName, fmCreate);
      fomCreate : begin
         Result:=TFileStream.Create(validFileName, fmCreate);
         if Result.Size>0 then
            Result.Size:=0;
      end;
   else
      Assert(False);
      Result:=nil;
   end;
end;

// ------------------
// ------------------ TdwsRestrictedOSFileSystem ------------------
// ------------------

// Create
//
constructor TdwsRestrictedOSFileSystem.Create;
begin
   inherited;
   FPaths:=TStringList.Create;
   TStringList(FPaths).OnChange:=PathsChanged;
end;

// Destroy
//
destructor TdwsRestrictedOSFileSystem.Destroy;
begin
   FPaths.Free;
   inherited;
end;

// SetPaths
//
procedure TdwsRestrictedOSFileSystem.SetPaths(const val : TStrings);
begin
   FPaths.Assign(val);
end;

// PathsChanged
//
procedure TdwsRestrictedOSFileSystem.PathsChanged(Sender : TObject);
begin
   FPathsPrepared:=False;
end;

// PreparePaths
//
procedure TdwsRestrictedOSFileSystem.PreparePaths;
const
   cDummyFileName = 'dummy.file';
var
   i : Integer;
   buf : UnicodeString;
begin
   if FPathsPrepared then Exit;
   for i:=FPaths.Count-1 downto 0 do begin
      buf:=Trim(FPaths[i]);
      if buf='' then
         FPaths.Delete(i)
      else begin
         buf:=ExpandFileName(IncludeTrailingPathDelimiter(buf)+cDummyFileName);
         FPaths[i]:=Copy(buf, 1, Length(buf)-Length(cDummyFileName));
      end;
   end;
   FPathsPrepared:=True;
end;

// ValidateFileName
//
function TdwsRestrictedOSFileSystem.ValidateFileName(const fileName : UnicodeString) : UnicodeString;
var
   i : Integer;
   path : UnicodeString;
begin
   for i:=0 to FPaths.Count-1 do begin
      path:=FPaths[i];
      Result:=ExpandFileName(path+fileName);
      if UnicodeSameText(path, Copy(Result, 1, Length(path))) then Exit;
   end;
   raise EdwsFSInvalidFileName.Create(fileName);
end;

// ------------------
// ------------------ TdwsNoFileSystem ------------------
// ------------------

// AllocateFileSystem
//
function TdwsNoFileSystem.AllocateFileSystem : IdwsFileSystem;
begin
   Result:=TdwsNullFileSystem.Create;
end;

// ------------------
// ------------------ TdwsRestrictedFileSystem ------------------
// ------------------

// Create
//
constructor TdwsRestrictedFileSystem.Create(Owner : TComponent);
begin
   inherited;
   FPaths:=TStringList.Create;
end;

// Destroy
//
destructor TdwsRestrictedFileSystem.Destroy;
begin
   inherited;
   FPaths.Free;
end;

// AllocateFileSystem
//
function TdwsRestrictedFileSystem.AllocateFileSystem : IdwsFileSystem;
var
   fs : TdwsRestrictedOSFileSystem;
begin
   fs:=TdwsRestrictedOSFileSystem.Create;
   fs.Paths:=Paths;
   Result:=fs;
end;

// SetPaths
//
procedure TdwsRestrictedFileSystem.SetPaths(const val : TStrings);
begin
   FPaths.Assign(val);
end;

end.
