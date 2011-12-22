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
unit dwsUnitSymbols;

{$I dws.inc}

interface

uses SysUtils, dwsUtils, dwsSymbols, dwsErrors, dwsStack, dwsXPlatform,
   dwsStrings;

type

   TSystemSymbolTable = class;
   TUnitMainSymbol = class;

   // list of unit main symbols (one per prog)
   TUnitMainSymbols = class(TObjectList<TUnitMainSymbol>)
      private

      protected

      public
         procedure Initialize(const msgs : TdwsCompileMessageList);

         function Find(const unitName : UnicodeString) : TUnitMainSymbol;
   end;

   IObjectOwner = interface
      procedure ReleaseObject;
   end;

   // TUnitSymbolTable
   //
   TUnitSymbolTable = class (TSymbolTable)
      private
         FObjects : TTightList;
         FUnitSymbol : TUnitMainSymbol;

      public
         destructor Destroy; override;

         procedure AddObjectOwner(const AOwner : IObjectOwner);
         procedure ClearObjectOwners;

         property UnitSymbol : TUnitMainSymbol read FUnitSymbol write FUnitSymbol;
   end;

   // TUnitPrivateTable
   //
   TUnitPrivateTable = class(TSymbolTable)
      private
         FUnitMainSymbol : TUnitMainSymbol;

      public
         constructor Create(unitMainSymbol : TUnitMainSymbol);

         property UnitMainSymbol : TUnitMainSymbol read FUnitMainSymbol;
   end;

   // TUnitImplementationTable
   //
   TUnitImplementationTable = class(TUnitPrivateTable)
      public
         constructor Create(unitMainSymbol : TUnitMainSymbol);
   end;

   TUnitSymbol = class;

   // Invisible symbol for source code
   TSourceSymbol = class (TSymbol)
      public
   end;

   // Invisible symbol for units (e. g. for TdwsUnit)
   TUnitMainSymbol = class sealed (TSourceSymbol)
      private
         FTable : TUnitSymbolTable;
         FInterfaceTable : TSymbolTable;
         FImplementationTable : TUnitImplementationTable;

      public
         constructor Create(const name : UnicodeString; table : TUnitSymbolTable;
                            unitSyms : TUnitMainSymbols);
         destructor Destroy; override;

         procedure Initialize(const msgs : TdwsCompileMessageList); override;

         procedure CreateInterfaceTable;
         procedure UnParentInterfaceTable;

         function ReferenceInSymbolTable(aTable : TSymbolTable) : TUnitSymbol;

         function HasSymbol(sym : TSymbol) : Boolean;

         property Table : TUnitSymbolTable read FTable;

         property InterfaceTable : TSymbolTable read FInterfaceTable;
         property ImplementationTable : TUnitImplementationTable read FImplementationTable;
   end;

   // Invisible symbol for included source code
   TIncludeSymbol = class (TSourceSymbol)
      public
         constructor Create(const fileName : UnicodeString);
   end;

   // Front end for units, serves for explicit unit resolution "unitName.symbolName"
   TUnitSymbol = class sealed (TTypeSymbol)
      private
         FMain : TUnitMainSymbol;
         FNameSpace : TFastCompareTextList;

      public
         constructor Create(mainSymbol : TUnitMainSymbol; const name : UnicodeString);
         destructor Destroy; override;

         procedure InitData(const data : TData; offset : Integer); override;

         procedure RegisterNameSpaceUnit(unitSymbol : TUnitSymbol);
         function  FindNameSpaceUnit(const name : UnicodeString) : TUnitSymbol;
         function  PossibleNameSpace(const name : UnicodeString) : Boolean;

         property Main : TUnitMainSymbol read FMain write FMain;

         function Table : TUnitSymbolTable; inline;
         function InterfaceTable : TSymbolTable; inline;
         function ImplementationTable : TUnitImplementationTable; inline;
   end;


   TStaticSymbolTable = class;

   IStaticSymbolTable = interface
      function SymbolTable : TStaticSymbolTable;
   end;

   // TStaticSymbolTable
   //
   TStaticSymbolTable = class (TUnitSymbolTable, IStaticSymbolTable)
      private
         FRefCount : Integer;
         FInitialized : Boolean;

      protected
         function SymbolTable : TStaticSymbolTable;

      public
         destructor Destroy; override;

         procedure Initialize(const msgs : TdwsCompileMessageList); override;
         procedure InsertParent(index : Integer; parent : TSymbolTable); override;
         function  RemoveParent(parent : TSymbolTable) : Integer; override;
         procedure ClearParents; override;

         function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
         function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
         function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
   end;

   // TLinkedSymbolTable
   //
   TLinkedSymbolTable = class (TUnitSymbolTable)
      private
         FParent : IStaticSymbolTable;
         FParentSymbolTable : TStaticSymbolTable;

      public
         constructor Create(const parent : IStaticSymbolTable);

         function FindLocal(const Name: UnicodeString; ofClass : TSymbolClass = nil) : TSymbol; override;
         function FindSymbol(const Name: UnicodeString; minVisibility : TdwsVisibility; ofClass : TSymbolClass = nil) : TSymbol; override;
         procedure Initialize(const msgs : TdwsCompileMessageList); override;

         property Parent : IStaticSymbolTable read FParent;
         property ParentSymbolTable : TStaticSymbolTable read FParentSymbolTable;
   end;

   ISystemSymbolTable = interface(IStaticSymbolTable)
      function SymbolTable : TSystemSymbolTable;
   end;

   // TSystemSymbolTable
   //
   TSystemSymbolTable = class (TStaticSymbolTable, ISystemSymbolTable)
      private
         FTypInteger : TBaseIntegerSymbol;
         FTypBoolean : TBaseBooleanSymbol;
         FTypFloat : TBaseFloatSymbol;
         FTypString : TBaseStringSymbol;
         FTypVariant : TBaseVariantSymbol;
         FTypObject : TClassSymbol;
         FTypClass : TClassOfSymbol;
         FTypException : TClassSymbol;
         FTypInterface : TInterfaceSymbol;

      protected
         function SymbolTable : TSystemSymbolTable;

      public
         property TypInteger : TBaseIntegerSymbol read FTypInteger write FTypInteger;
         property TypBoolean : TBaseBooleanSymbol read FTypBoolean write FTypBoolean;
         property TypFloat : TBaseFloatSymbol read FTypFloat write FTypFloat;
         property TypString : TBaseStringSymbol read FTypString write FTypString;
         property TypVariant : TBaseVariantSymbol read FTypVariant write FTypVariant;

         property TypObject : TClassSymbol read FTypObject write FTypObject;
         property TypClass : TClassOfSymbol read FTypClass write FTypClass;

         property TypException : TClassSymbol read FTypException write FTypException;

         property TypInterface : TInterfaceSymbol read FTypInterface write FTypInterface;
   end;

   // TProgramSymbolTable
   //
   TProgramSymbolTable = class (TSymbolTable)
      private
         FDestructionList : TTightList;

      public
         destructor Destroy; override;

         procedure AddToDestructionList(sym : TSymbol);
         procedure RemoveFromDestructionList(sym : TSymbol);
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TStaticSymbolTable ------------------
// ------------------

// Destroy
//
destructor TStaticSymbolTable.Destroy;
begin
   Assert(FRefCount=0);
   ClearParents;
   inherited;
end;

// _AddRef
//
function TStaticSymbolTable._AddRef : longint;
begin
   Result:=InterlockedIncrement(FRefCount);
end;

// _Release
//
function TStaticSymbolTable._Release : longint;
begin
   Result:=InterlockedDecrement(FRefCount);
   if Result=0 then
      Destroy;
end;

// InsertParent
//
procedure TStaticSymbolTable.InsertParent(index : Integer; parent : TSymbolTable);
var
   staticTable : TStaticSymbolTable;
begin
   // accept only static parents
   Assert((parent is TStaticSymbolTable), CPE_NoStaticSymbols);

   staticTable:=TStaticSymbolTable(parent);
   staticTable._AddRef;
   inherited InsertParent(index, staticTable);
end;

// RemoveParent
//
function TStaticSymbolTable.RemoveParent(parent : TSymbolTable) : Integer;
begin
   (parent as TStaticSymbolTable)._Release;
   Result:=inherited RemoveParent(parent);
end;

// ClearParents
//
procedure TStaticSymbolTable.ClearParents;
var
   i : Integer;
begin
   for i:=0 to ParentCount-1 do
      TStaticSymbolTable(Parents[i])._Release;
   inherited;
end;

// QueryInterface
//
function TStaticSymbolTable.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;
begin
   if GetInterface(IID, Obj) then
      Result:=0
   else Result:=E_NOINTERFACE;
end;

// Initialize
//
procedure TStaticSymbolTable.Initialize(const msgs : TdwsCompileMessageList);
begin
   if not FInitialized then begin
      inherited;
      FInitialized:=True;
   end;
end;

// SymbolTable
//
function TStaticSymbolTable.SymbolTable : TStaticSymbolTable;
begin
   Result:=Self;
end;

// ------------------
// ------------------ TLinkedSymbolTable ------------------
// ------------------

// Create
//
constructor TLinkedSymbolTable.Create(const parent : IStaticSymbolTable);
begin
   inherited Create(nil, nil);
   FParent:=parent;
   FParentSymbolTable:=parent.SymbolTable;
end;

function TLinkedSymbolTable.FindLocal(const Name: UnicodeString; ofClass : TSymbolClass = nil): TSymbol;
begin
   Result:=FParentSymbolTable.FindLocal(Name, ofClass);
   if not Assigned(Result) then
      Result:=inherited FindLocal(Name, ofClass);
end;

function TLinkedSymbolTable.FindSymbol(const Name: UnicodeString; minVisibility : TdwsVisibility;
                                       ofClass : TSymbolClass = nil): TSymbol;
begin
  Result := FParentSymbolTable.FindSymbol(Name, minVisibility, ofClass);
  if not Assigned(Result) then
    Result := inherited FindSymbol(Name, minVisibility, ofClass);
end;

procedure TLinkedSymbolTable.Initialize(const msgs : TdwsCompileMessageList);
begin
  FParentSymbolTable.Initialize(msgs);
  inherited;
end;

// ------------------
// ------------------ TUnitMainSymbol ------------------
// ------------------

// Create
//
constructor TUnitMainSymbol.Create(const name : UnicodeString; table : TUnitSymbolTable;
                                   unitSyms : TUnitMainSymbols);
begin
   inherited Create(Name, nil);
   FTable:=Table;
   unitSyms.Add(Self);
end;

// Destroy
//
destructor TUnitMainSymbol.Destroy;
begin
   FInterfaceTable.Free;
   FImplementationTable.Free;
   FTable.Free;
   inherited;
end;

// Initialize
//
procedure TUnitMainSymbol.Initialize(const msgs : TdwsCompileMessageList);
begin
   FTable.Initialize(msgs);
   if FImplementationTable<>nil then
      FImplementationTable.Initialize(msgs);
end;

// CreateInterfaceTable
//
procedure TUnitMainSymbol.CreateInterfaceTable;
begin
   Assert(not Assigned(FInterfaceTable));

   FInterfaceTable:=TSymbolTable.Create;
   Table.AddParent(FInterfaceTable);
end;

// UnParentInterfaceTable
//
procedure TUnitMainSymbol.UnParentInterfaceTable;
begin
   Table.RemoveParent(FInterfaceTable);
end;

// HasSymbol
//
function TUnitMainSymbol.HasSymbol(sym : TSymbol) : Boolean;
begin
   if Self=nil then
      Result:=False
   else Result:=Table.HasSymbol(sym) or ImplementationTable.HasSymbol(sym);
end;

// ReferenceInSymbolTable
//
function TUnitMainSymbol.ReferenceInSymbolTable(aTable : TSymbolTable) : TUnitSymbol;
var
   p : Integer;
   nameSpace : TUnitSymbol;
   part : String;
begin
   p:=Pos('.', Name);
   if p>0 then
      part:=Copy(Name, 1, p-1)
   else part:=Name;

   nameSpace:=TUnitSymbol(aTable.FindLocal(part, TUnitSymbol));
   if nameSpace=nil then begin
      nameSpace:=TUnitSymbol.Create(nil, part);
      aTable.AddSymbol(nameSpace);
   end;

   if p>0 then begin
      Result:=TUnitSymbol.Create(Self, Name);
      aTable.AddSymbol(Result);
      nameSpace.RegisterNameSpaceUnit(Result);
   end else begin
      Result:=nameSpace;
      if not ((nameSpace.Main=nil) or (nameSpace.Main=Self)) then
         Assert((nameSpace.Main=nil) or (nameSpace.Main=Self));
      nameSpace.Main:=Self;
   end;

   aTable.AddParent(Table);
end;

// ------------------
// ------------------ TIncludeSymbol ------------------
// ------------------

// Create
//
constructor TIncludeSymbol.Create(const fileName : UnicodeString);
begin
   inherited Create('$i '+fileName, nil);
end;

// ------------------
// ------------------ TUnitMainSymbols ------------------
// ------------------

// Find
//
function TUnitMainSymbols.Find(const unitName : UnicodeString) : TUnitMainSymbol;
var
   i : Integer;
begin
   for i:=0 to Count-1 do begin
      Result:=Items[i];
      if UnicodeSameText(Result.Name, unitName) then
         Exit(Result);
   end;
   Result:=nil;
end;

// Initialize
//
procedure TUnitMainSymbols.Initialize(const msgs : TdwsCompileMessageList);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].Initialize(msgs);
end;

// ------------------
// ------------------ TUnitSymbolTable ------------------
// ------------------

// Destroy
//
destructor TUnitSymbolTable.Destroy;
begin
   ClearObjectOwners;
   inherited;
   FObjects.Free;
end;

// AddObjectOwner
//
procedure TUnitSymbolTable.AddObjectOwner(const AOwner : IObjectOwner);
begin
   AOwner._AddRef;
   FObjects.Add(Pointer(AOwner));
end;

// ClearObjectOwners
//
procedure TUnitSymbolTable.ClearObjectOwners;
var
   i : Integer;
   objOwner : Pointer;
begin
   for i:=0 to FObjects.Count-1 do begin
      objOwner:=FObjects.List[i];
      IObjectOwner(objOwner).ReleaseObject;
      IObjectOwner(objOwner)._Release;
   end;
   FObjects.Clear;
end;

// ------------------
// ------------------ TUnitSymbol ------------------
// ------------------

// Create
//
constructor TUnitSymbol.Create(mainSymbol : TUnitMainSymbol; const name : UnicodeString);
begin
   inherited Create(name, nil);
   FMain:=mainSymbol;
end;

// Destroy
//
destructor TUnitSymbol.Destroy;
begin
   inherited;
   FNameSpace.Free;
end;

// InitData
//
procedure TUnitSymbol.InitData(const data : TData; offset : Integer);
begin
   // nothing
end;

// RegisterNameSpaceUnit
//
procedure TUnitSymbol.RegisterNameSpaceUnit(unitSymbol : TUnitSymbol);
begin
   if FNameSpace=nil then begin
      FNameSpace:=TFastCompareTextList.Create;
      FNameSpace.Sorted:=True;
   end;
   FNameSpace.AddObject(unitSymbol.Name, unitSymbol);
end;

// FindNameSpaceUnit
//
function TUnitSymbol.FindNameSpaceUnit(const name : UnicodeString) : TUnitSymbol;
var
   i : Integer;
begin
   Result:=nil;
   if (Main<>nil) and UnicodeSameText(name, Self.Name) then
      Result:=Self
   else if FNameSpace<>nil then begin
      i:=FNameSpace.IndexOf(name);
      if i>=0 then
         Result:=TUnitSymbol(FNameSpace.Objects[i]);
   end;
end;

// PossibleNameSpace
//
function TUnitSymbol.PossibleNameSpace(const name : UnicodeString) : Boolean;
var
   i : Integer;
   candidate : String;
begin
   if FNameSpace=nil then Exit(False);
   if FNameSpace.Find(name, i) then Exit(True);
   if Cardinal(i)>=Cardinal(FNameSpace.Count) then Exit(False);
   candidate:=FNameSpace[i];
   Result:=    (Length(candidate)>Length(name))
           and (StrIBeginsWith(candidate, name))
           and (candidate[Length(name)+1]='.');
end;

// Table
//
function TUnitSymbol.Table : TUnitSymbolTable;
begin
   if Main<>nil then
      Result:=Main.Table
   else Result:=nil;
end;

// InterfaceTable
//
function TUnitSymbol.InterfaceTable : TSymbolTable;
begin
   if Main<>nil then
      Result:=Main.InterfaceTable
   else Result:=nil;
end;

// ImplementationTable
//
function TUnitSymbol.ImplementationTable : TUnitImplementationTable;
begin
   if Main<>nil then
      Result:=Main.ImplementationTable
   else Result:=nil;
end;

// ------------------
// ------------------ TUnitPrivateTable ------------------
// ------------------

// Create
//
constructor TUnitPrivateTable.Create(unitMainSymbol : TUnitMainSymbol);
begin
   inherited Create(unitMainSymbol.Table, unitMainSymbol.Table.AddrGenerator);
   FUnitMainSymbol:=unitMainSymbol;
end;

// ------------------
// ------------------ TUnitImplementationTable ------------------
// ------------------

// Create
//
constructor TUnitImplementationTable.Create(unitMainSymbol : TUnitMainSymbol);
begin
   inherited Create(unitMainSymbol);
   unitMainSymbol.FImplementationTable:=Self;
   AddParent(unitMainSymbol.InterfaceTable);
end;

// ------------------
// ------------------ TSystemSymbolTable ------------------
// ------------------

// SymbolTable
//
function TSystemSymbolTable.SymbolTable : TSystemSymbolTable;
begin
   Result:=Self;
end;

// ------------------
// ------------------ TProgramSymbolTable ------------------
// ------------------

// Destroy
//
destructor TProgramSymbolTable.Destroy;
begin
   inherited;
   FDestructionList.Clean;
end;

// AddToDestructionList
//
procedure TProgramSymbolTable.AddToDestructionList(sym : TSymbol);
begin
   FDestructionList.Add(sym);
end;

// RemoveFromDestructionList
//
procedure TProgramSymbolTable.RemoveFromDestructionList(sym : TSymbol);
begin
   FDestructionList.Remove(sym);
end;

end.
