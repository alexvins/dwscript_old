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
{    Eric Grange                                                       }
{                                                                      }
{**********************************************************************}
unit dwsCodeGen;

{$i dws.inc}

interface

uses Classes, SysUtils, dwsUtils, dwsSymbols, dwsExprs, dwsCoreExprs, dwsJSON,
   dwsStrings, dwsXPlatform;

   // experimental codegen support classes for DWScipt

type

   TdwsExprCodeGen = class;

   TdwsMappedSymbol = record
      Symbol : TSymbol;
      Name : UnicodeString;
   end;

   TdwsMappedSymbolHash = class(TSimpleHash<TdwsMappedSymbol>)
      protected
         function SameItem(const item1, item2 : TdwsMappedSymbol) : Boolean; override;
         function GetItemHashCode(const item1 : TdwsMappedSymbol) : Integer; override;
   end;

   TdwsCodeGenSymbolScope = (cgssGlobal, cgssClass, cgssLocal);

   TdwsCodeGenSymbolMaps = class;

   TdwsCodeGenSymbolMap = class
      private
         FParent : TdwsCodeGenSymbolMap;
         FSymbol : TSymbol;
         FHash : TdwsMappedSymbolHash;
         FMaps : TdwsCodeGenSymbolMaps;
         FNames : TStringList;
         FLookup : TdwsMappedSymbol;
         FReservedSymbol : TSymbol;
         FPrefix : UnicodeString;

      protected
         function DoNeedUniqueName(symbol : TSymbol; tryCount : Integer; canObfuscate : Boolean) : UnicodeString; virtual;

      public
         constructor Create(aParent : TdwsCodeGenSymbolMap; aSymbol : TSymbol);
         destructor Destroy; override;

         function SymbolToName(symbol : TSymbol) : UnicodeString;
         function NameToSymbol(const name : UnicodeString; scope : TdwsCodeGenSymbolScope) : TSymbol;

         procedure ReserveName(const name : UnicodeString); inline;
         procedure ReserveExternalName(sym : TSymbol); inline;

         function MapSymbol(symbol : TSymbol; scope : TdwsCodeGenSymbolScope; canObfuscate : Boolean) : UnicodeString;
         procedure ForgetSymbol(symbol : TSymbol);

         property Maps : TdwsCodeGenSymbolMaps read FMaps write FMaps;
         property Parent : TdwsCodeGenSymbolMap read FParent;
         property Prefix : UnicodeString read FPrefix write FPrefix;
         property Symbol : TSymbol read FSymbol;
   end;

   TdwsCodeGenSymbolMaps = class(TObjectList<TdwsCodeGenSymbolMap>)
      private

      protected

      public
         function MapOf(symbol : TSymbol) : TdwsCodeGenSymbolMap;
   end;

   TdwsRegisteredCodeGen = class
      public
         Expr : TExprBaseClass;
         CodeGen : TdwsExprCodeGen;

         destructor Destroy; override;
   end;

   TdwsRegisteredCodeGenList = class(TSortedList<TdwsRegisteredCodeGen>)
      protected
         function Compare(const item1, item2 : TdwsRegisteredCodeGen) : Integer; override;
   end;

   TdwsCodeGenOption = (cgoNoRangeChecks, cgoNoCheckInstantiated, cgoNoCheckLoopStep,
                        cgoNoConditions, cgoNoInlineMagics, cgoObfuscate, cgoNoSourceLocations,
                        cgoOptimizeForSize);
   TdwsCodeGenOptions = set of TdwsCodeGenOption;

   TdwsCodeGen = class
      private
         FCodeGenList : TdwsRegisteredCodeGenList;
         FOutput : TWriteOnlyBlockStream;
         FDependencies : TStringList;
         FFlushedDependencies : TStringList;
         FTempReg : TdwsRegisteredCodeGen;

         FLocalTable : TSymbolTable;
         FTableStack : TTightStack;

         FContext : TdwsProgram;
         FContextStack : TTightStack;

         FLocalVarSymbolMap : TStringList;
         FLocalVarSymbolMapStack : TTightStack;

         FSymbolMap : TdwsCodeGenSymbolMap;
         FSymbolMaps : TdwsCodeGenSymbolMaps;
         FSymbolMapStack : TTightStack;
         FMappedUnits : TTightList;

         FTempSymbolCounter : Integer;
         FCompiledClasses : TTightList;
         FCompiledUnits : TTightList;
         FIndent : Integer;
         FIndentString : UnicodeString;
         FNeedIndent : Boolean;
         FIndentSize : Integer;
         FOptions : TdwsCodeGenOptions;

      protected
         procedure EnterContext(proc : TdwsProgram); virtual;
         procedure LeaveContext; virtual;

         function CreateSymbolMap(parentMap : TdwsCodeGenSymbolMap; symbol : TSymbol) : TdwsCodeGenSymbolMap; virtual;

         procedure EnterScope(symbol : TSymbol);
         procedure LeaveScope;
         function  EnterStructScope(struct : TStructuredTypeSymbol) : Integer;
         procedure LeaveScopes(n : Integer);
         function  IsScopeLevel(symbol : TSymbol) : Boolean;

         procedure RaiseUnknowExpression(expr : TExprBase);

         procedure DoCompileClassSymbol(cls : TClassSymbol); virtual;
         procedure DoCompileFuncSymbol(func : TSourceFuncSymbol); virtual;
         procedure DoCompileUnitSymbol(un : TUnitMainSymbol); virtual;

         procedure MapStructuredSymbol(structSym : TStructuredTypeSymbol; canObfuscate : Boolean);

      public
         constructor Create; virtual;
         destructor Destroy; override;

         procedure RegisterCodeGen(expr : TExprBaseClass; codeGen : TdwsExprCodeGen);
         function  FindCodeGen(expr : TExprBase) : TdwsExprCodeGen;
         function  FindSymbolAtStackAddr(stackAddr, level : Integer) : TDataSymbol;
         function  SymbolMappedName(sym : TSymbol; scope : TdwsCodeGenSymbolScope) : UnicodeString; virtual;

         procedure Compile(expr : TExprBase);
         procedure CompileNoWrap(expr : TTypedExpr);

         procedure CompileSymbolTable(table : TSymbolTable); virtual;
         procedure CompileUnitSymbol(un : TUnitMainSymbol);
         procedure CompileEnumerationSymbol(enum : TEnumerationSymbol); virtual;
         procedure CompileFuncSymbol(func : TSourceFuncSymbol);
         procedure CompileConditions(func : TFuncSymbol; conditions : TSourceConditions;
                                     preConds : Boolean); virtual;
         procedure CompileRecordSymbol(rec : TRecordSymbol); virtual;
         procedure CompileClassSymbol(cls : TClassSymbol);
         procedure BeforeCompileProgram(table, systemTable : TSymbolTable; unitSyms : TUnitMainSymbols);
         procedure CompileProgram(const prog : IdwsProgram); virtual;
         procedure CompileProgramInSession(const prog : IdwsProgram); virtual;
         procedure CompileProgramBody(expr : TNoResultExpr); virtual;

         procedure BeginProgramSession(const prog : IdwsProgram); virtual;
         procedure EndProgramSession; virtual;

         procedure ReserveSymbolNames; virtual;
         procedure MapInternalSymbolNames(progTable, systemTable : TSymbolTable); virtual;
         procedure MapPrioritySymbolNames(table : TSymbolTable); virtual;
         procedure MapNormalSymbolNames(table : TSymbolTable); virtual;

         procedure CompileDependencies(destStream : TWriteOnlyBlockStream; const prog : IdwsProgram); virtual;

         procedure WriteIndent;
         procedure Indent;
         procedure UnIndent;

         procedure WriteString(const s : UnicodeString); overload;
         procedure WriteString(const c : WideChar); overload;
         procedure WriteStringLn(const s : UnicodeString);
         procedure WriteLineEnd;

         procedure WriteSymbolName(sym : TSymbol; scope : TdwsCodeGenSymbolScope = cgssGlobal);

         function LocationString(e : TExprBase) : UnicodeString;
         function GetNewTempSymbol : UnicodeString; virtual;

         procedure WriteCompiledOutput(dest : TWriteOnlyBlockStream; const prog : IdwsProgram); virtual;
         function CompiledOutput(const prog : IdwsProgram) : UnicodeString;
         procedure FushDependencies;

         procedure Clear; virtual;

         property Context : TdwsProgram read FContext;
         property LocalTable : TSymbolTable read FLocalTable write FLocalTable;
         property SymbolMap : TdwsCodeGenSymbolMap read FSymbolMap;

         property IndentSize : Integer read FIndentSize write FIndentSize;
         property Options : TdwsCodeGenOptions read FOptions write FOptions;

         property Output : TWriteOnlyBlockStream read FOutput;
         property Dependencies : TStringList read FDependencies;
         property FlushedDependencies : TStringList read FFlushedDependencies;
   end;

   TdwsExprCodeGen = class abstract
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); virtual;
         procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); virtual;
   end;

   TdwsExprGenericCodeGen = class(TdwsExprCodeGen)
      private
         FTemplate : array of TVarRec;
         FStatement : Boolean;
         FUnWrapable : Boolean;
         FDependency : UnicodeString;

      protected
         procedure DoCodeGen(codeGen : TdwsCodeGen; expr : TExprBase; start, stop : Integer);

      public
         constructor Create(const template : array of const; statement : Boolean = False;
                            const dependency : UnicodeString = ''); overload;

         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
         procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   ECodeGenException = class (Exception);
   ECodeGenUnknownExpression = class (ECodeGenException);
   ECodeGenUnsupportedSymbol = class (ECodeGenException);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsRegisteredCodeGen ------------------
// ------------------

// Destroy
//
destructor TdwsRegisteredCodeGen.Destroy;
begin
   inherited;
   CodeGen.Free;
end;

// ------------------
// ------------------ TdwsRegisteredCodeGenList ------------------
// ------------------

// Compare
//
function TdwsRegisteredCodeGenList.Compare(const item1, item2 : TdwsRegisteredCodeGen) : Integer;
var
   i1, i2 : Integer;
begin
   i1:=NativeInt(item1.Expr);
   i2:=NativeInt(item2.Expr);
   if i1<i2 then
      Result:=-1
   else if i1=i2 then
      Result:=0
   else Result:=1;
end;

// ------------------
// ------------------ TdwsCodeGen ------------------
// ------------------

// Create
//
constructor TdwsCodeGen.Create;
begin
   inherited;
   FCodeGenList:=TdwsRegisteredCodeGenList.Create;
   FOutput:=TWriteOnlyBlockStream.Create;
   FDependencies:=TStringList.Create;
   FDependencies.Sorted:=True;
   FDependencies.Duplicates:=dupIgnore;
   FFlushedDependencies:=TStringList.Create;
   FFlushedDependencies.Sorted:=True;
   FFlushedDependencies.Duplicates:=dupIgnore;
   FTempReg:=TdwsRegisteredCodeGen.Create;
   FSymbolMaps:=TdwsCodeGenSymbolMaps.Create;
   FIndentSize:=3;
end;

// Destroy
//
destructor TdwsCodeGen.Destroy;
begin
   Clear;
   FSymbolMapStack.Free;
   FMappedUnits.Free;
   FSymbolMaps.Free;
   FTempReg.Free;
   FDependencies.Free;
   FFlushedDependencies.Free;
   FOutput.Free;
   FCodeGenList.Clean;
   FCodeGenList.Free;
   FTableStack.Free;
   FContextStack.Free;
   FCompiledClasses.Free;
   FCompiledUnits.Free;
   FLocalVarSymbolMapStack.Free;
   inherited;
end;

// RegisterCodeGen
//
procedure TdwsCodeGen.RegisterCodeGen(expr : TExprBaseClass; codeGen : TdwsExprCodeGen);
var
   reg : TdwsRegisteredCodeGen;
begin
   reg:=TdwsRegisteredCodeGen.Create;
   reg.Expr:=expr;
   reg.CodeGen:=codeGen;
   FCodeGenList.Add(reg);
end;

// FindCodeGen
//
function TdwsCodeGen.FindCodeGen(expr : TExprBase) : TdwsExprCodeGen;
var
   i : Integer;
begin
   FTempReg.Expr:=TExprBaseClass(expr.ClassType);
   if FCodeGenList.Find(FTempReg, i) then
      Result:=FCodeGenList.Items[i].CodeGen
   else Result:=nil;
end;

// FindSymbolAtStackAddr
//
function TdwsCodeGen.FindSymbolAtStackAddr(stackAddr, level : Integer) : TDataSymbol;
var
   i : Integer;
   funcSym : TFuncSymbol;
   dataSym : TDataSymbol;
begin
   if (Context is TdwsProcedure) then begin
      funcSym:=TdwsProcedure(Context).Func;
      Result:=funcSym.Result;
      if (Result<>nil) and (Result.StackAddr=stackAddr) then
         Exit;
   end;

   for i:=0 to FLocalVarSymbolMap.Count-1 do begin
      dataSym:=TDataSymbol(FLocalVarSymbolMap.Objects[i]);
      if (dataSym.StackAddr=stackAddr) and (dataSym.Level=level) then begin
         Result:=dataSym;
         Exit;
      end;
   end;

   if FLocalTable=nil then Exit(nil);
   Result:=FLocalTable.FindSymbolAtStackAddr(stackAddr, level);
end;

// SymbolMappedName
//
function TdwsCodeGen.SymbolMappedName(sym : TSymbol; scope : TdwsCodeGenSymbolScope) : UnicodeString;
var
   i : Integer;
begin
   if sym is TFuncSymbol then begin
      if TFuncSymbol(sym).IsExternal then
         Exit(sym.Name);
      if (sym is TMethodSymbol) then begin
         while TMethodSymbol(sym).IsOverride do
            sym:=TMethodSymbol(sym).ParentMeth;
      end;
   end else if sym is TClassSymbol then begin
      if TClassSymbol(sym).IsExternal then
         Exit(sym.Name);
   end;
   Result:=FSymbolMap.SymbolToName(sym);
   if Result<>'' then Exit;
   for i:=0 to FSymbolMaps.Count-1 do begin
      Result:=FSymbolMaps[i].SymbolToName(sym);
      if Result<>'' then Exit;
   end;
   Result:=FSymbolMap.MapSymbol(sym, scope, True);
end;

// Clear
//
procedure TdwsCodeGen.Clear;
begin
   FOutput.Clear;
   FDependencies.Clear;
   FFlushedDependencies.Clear;

   FLocalTable:=nil;
   FTableStack.Clear;
   FContext:=nil;
   FContextStack.Clear;
   FCompiledClasses.Clear;
   FCompiledUnits.Clear;
   FMappedUnits.Clear;

   FSymbolMap:=nil;
   FSymbolMaps.Clear;
   EnterScope(nil);

   FIndent:=0;
   FIndentString:='';
   FNeedIndent:=False;

   FTempSymbolCounter:=0;
end;

// Compile
//
procedure TdwsCodeGen.Compile(expr : TExprBase);
var
   cg : TdwsExprCodeGen;
   oldTable : TSymbolTable;
begin
   if expr=nil then Exit;
   cg:=FindCodeGen(expr);
   if cg=nil then
      RaiseUnknowExpression(expr);

   if expr.InheritsFrom(TBlockExpr) then begin
      FTableStack.Push(FLocalTable);
      oldTable:=FLocalTable;
      FLocalTable:=TBlockExpr(expr).Table;
      try
         cg.CodeGen(Self, expr);
      finally
         FLocalTable:=oldTable;
         FTableStack.Pop;
      end;
   end else begin
      cg.CodeGen(Self, expr);
   end;
end;

// CompileNoWrap
//
procedure TdwsCodeGen.CompileNoWrap(expr : TTypedExpr);
var
   cg : TdwsExprCodeGen;
begin
   cg:=FindCodeGen(expr);
   if cg=nil then
      RaiseUnknowExpression(expr);

   cg.CodeGenNoWrap(Self, expr)
end;

// CompileSymbolTable
//
procedure TdwsCodeGen.CompileSymbolTable(table : TSymbolTable);
var
   sym : TSymbol;
begin
   for sym in table do begin
      if sym is TSourceFuncSymbol then
         CompileFuncSymbol(TSourceFuncSymbol(sym))
      else if sym is TEnumerationSymbol then
         CompileEnumerationSymbol(TEnumerationSymbol(sym))
      else if sym is TRecordSymbol then
         CompileRecordSymbol(TRecordSymbol(sym))
      else if sym is TClassSymbol then begin
         if FCompiledClasses.IndexOf(sym)<0 then
            CompileClassSymbol(TClassSymbol(sym));
      end;
   end;
end;

// CompileUnitSymbol
//
procedure TdwsCodeGen.CompileUnitSymbol(un : TUnitMainSymbol);
begin
   if FCompiledUnits.IndexOf(un)>=0 then Exit;
   FCompiledUnits.Add(un);

   EnterScope(un);
   DoCompileUnitSymbol(un);
   LeaveScope;
end;

// DoCompileUnitSymbol
//
procedure TdwsCodeGen.DoCompileUnitSymbol(un : TUnitMainSymbol);
var
   oldTable : TSymbolTable;
begin
   CompileSymbolTable(un.Table);

   oldTable:=FLocalTable;
   FLocalTable:=un.ImplementationTable;
   try
      CompileSymbolTable(un.ImplementationTable);
   finally
      FLocalTable:=oldTable;
   end;
end;

// CompileEnumerationSymbol
//
procedure TdwsCodeGen.CompileEnumerationSymbol(enum : TEnumerationSymbol);
begin
   // nothing
end;

// CompileFuncSymbol
//
procedure TdwsCodeGen.CompileFuncSymbol(func : TSourceFuncSymbol);
var
   execSelf : TObject;
   proc : TdwsProcedure;
begin
   // nil executable means it's a function pointer type
   if func.Executable=nil then Exit;
   execSelf:=func.Executable.GetSelf;
   if not (execSelf is TdwsProcedure) then Exit;
   proc:=TdwsProcedure(execSelf);
   if proc<>nil then begin
      EnterScope(func);
      EnterContext(proc);
      try
         DoCompileFuncSymbol(func);
      finally
         LeaveContext;
         LeaveScope;
      end;
   end;
end;

// DoCompileFuncSymbol
//
procedure TdwsCodeGen.DoCompileFuncSymbol(func : TSourceFuncSymbol);
var
   proc : TdwsProcedure;
begin
   proc:=(func.Executable.GetSelf as TdwsProcedure);

   if not (cgoNoConditions in Options) then
      CompileConditions(func, proc.PreConditions, True);

   Assert(func.SubExprCount=2);
   Compile(func.SubExpr[0]);
   Compile(func.SubExpr[1]);

   if not (cgoNoConditions in Options) then
      CompileConditions(func, proc.PostConditions, False);
end;

// CompileConditions
//
procedure TdwsCodeGen.CompileConditions(func : TFuncSymbol; conditions : TSourceConditions;
                                        preConds : Boolean);
begin
   // nothing
end;

// CompileRecordSymbol
//
procedure TdwsCodeGen.CompileRecordSymbol(rec : TRecordSymbol);
begin
   // nothing here
end;

// CompileClassSymbol
//
procedure TdwsCodeGen.CompileClassSymbol(cls : TClassSymbol);
begin
   if cls.IsExternal then Exit;

   EnterScope(cls);
   try
      DoCompileClassSymbol(cls);
   finally
      LeaveScope;
   end;
end;

// BeforeCompileProgram
//
procedure TdwsCodeGen.BeforeCompileProgram(table, systemTable : TSymbolTable; unitSyms : TUnitMainSymbols);
var
   i : Integer;
begin
   ReserveSymbolNames;
   MapInternalSymbolNames(table, systemTable);

   for i:=0 to unitSyms.Count-1 do
      MapPrioritySymbolNames(unitSyms[i].Table);

   MapPrioritySymbolNames(table);
   MapNormalSymbolNames(table);
end;

// DoCompileClassSymbol
//
procedure TdwsCodeGen.DoCompileClassSymbol(cls : TClassSymbol);
begin
   if FCompiledClasses.IndexOf(cls.Parent)<0 then begin
      if     (cls.Parent.Name<>'TObject')
         and (cls.Parent.Name<>'Exception') then
         CompileClassSymbol(cls.Parent);
   end;
   FCompiledClasses.Add(cls);
end;

// CompileProgram
//
procedure TdwsCodeGen.CompileProgram(const prog : IdwsProgram);
var
   p : TdwsProgram;
begin
   p:=prog.ProgramObject;

   BeginProgramSession(prog);
   try
      BeforeCompileProgram(prog.Table, p.SystemTable, p.UnitMains);

      CompileProgramInSession(prog);
   finally
      EndProgramSession;
   end;
end;

// CompileProgramInSession
//
procedure TdwsCodeGen.CompileProgramInSession(const prog : IdwsProgram);
var
   p : TdwsProgram;
   i : Integer;
begin
   p:=prog.ProgramObject;

   for i:=0 to p.UnitMains.Count-1 do
      CompileUnitSymbol(p.UnitMains[i]);

   Compile(p.InitExpr);

   CompileSymbolTable(p.Table);

   if not (p.Expr is TNullExpr) then
      CompileProgramBody(p.Expr);
end;

// BeginProgramSession
//
procedure TdwsCodeGen.BeginProgramSession(const prog : IdwsProgram);
var
   p : TdwsProgram;
begin
   if FSymbolMap=nil then
      EnterScope(nil);

   p:=prog.ProgramObject;
   EnterContext(p);
end;

// EndProgramSession
//
procedure TdwsCodeGen.EndProgramSession;
begin
   LeaveContext;
end;

// ReserveSymbolNames
//
procedure TdwsCodeGen.ReserveSymbolNames;
begin
   // nothing
end;

// MapStructuredSymbol
//
procedure TdwsCodeGen.MapStructuredSymbol(structSym : TStructuredTypeSymbol; canObfuscate : Boolean);
var
   sym : TSymbol;
   n : Integer;
begin
   if FSymbolMaps.MapOf(structSym)<>nil then Exit;

   if structSym.Parent<>nil then
      MapStructuredSymbol(structSym.Parent, canObfuscate);

   if (structSym.UnitSymbol<>nil) and not IsScopeLevel(structSym.UnitSymbol) then begin
      EnterScope(structSym.UnitSymbol);
      SymbolMap.MapSymbol(structSym, cgssGlobal, canObfuscate);
      LeaveScope;
   end else SymbolMap.MapSymbol(structSym, cgssGlobal, canObfuscate);

   n:=EnterStructScope(structSym);
   if structSym is TRecordSymbol then begin
      for sym in structSym.Members do begin
         if (sym is TMethodSymbol) and (TMethodSymbol(sym).IsClassMethod) then
            SymbolMap.MapSymbol(sym, cgssGlobal, canObfuscate)
         else SymbolMap.MapSymbol(sym, cgssClass, canObfuscate);
      end;
   end else begin
      for sym in structSym.Members do
         SymbolMap.MapSymbol(sym, cgssClass, canObfuscate);
   end;
   LeaveScopes(n);
end;

// MapInternalSymbolNames
//
procedure TdwsCodeGen.MapInternalSymbolNames(progTable, systemTable : TSymbolTable);

   procedure MapSymbolTable(table : TSymbolTable);
   var
      i : Integer;
      sym : TSymbol;
   begin
      for i:=0 to table.Count-1 do begin
         sym:=table.Symbols[i];
         if (sym is TClassSymbol) or (sym is TRecordSymbol) then begin
            MapStructuredSymbol(TStructuredTypeSymbol(sym), False);
         end else if sym is TInterfaceSymbol then
            SymbolMap.MapSymbol(sym, cgssGlobal, False)
         else if sym is TFuncSymbol then
            SymbolMap.MapSymbol(sym, cgssGlobal, False)
         else if sym is TDataSymbol then
            SymbolMap.MapSymbol(sym, cgssGlobal, False);
      end;
   end;

var
   u : TUnitSymbol;
begin
   MapSymbolTable(systemTable);
   u:=TUnitSymbol(progTable.FindSymbol(SYS_INTERNAL, cvMagic, TUnitSymbol));
   if u<>nil then
      MapSymbolTable(u.Table);
   u:=TUnitSymbol(progTable.FindSymbol(SYS_DEFAULT, cvMagic, TUnitSymbol));
   if u<>nil then
      MapSymbolTable(u.Table);
end;

// MapPrioritySymbolNames
//
procedure TdwsCodeGen.MapPrioritySymbolNames(table : TSymbolTable);
var
   sym : TSymbol;
   unitSym : TUnitSymbol;
begin
   for sym in table do begin
      if sym is TUnitSymbol then begin
         unitSym:=TUnitSymbol(sym);
         if unitSym.Table is TStaticSymbolTable then
            MapPrioritySymbolNames(unitSym.Table);
      end else if sym is TClassSymbol then begin
         if TClassSymbol(sym).IsExternal then begin
            SymbolMap.ReserveExternalName(sym);
         end;
      end else if sym is TFuncSymbol then begin
         if TFuncSymbol(sym).IsExternal then
            SymbolMap.ReserveExternalName(sym);
      end;
   end;
end;

// MapNormalSymbolNames
//
procedure TdwsCodeGen.MapNormalSymbolNames(table : TSymbolTable);
var
   sym : TSymbol;
   unitSym : TUnitMainSymbol;
begin
   for sym in table do begin
      if sym is TUnitSymbol then begin
         unitSym:=TUnitSymbol(sym).Main;
         if not (unitSym.Table is TStaticSymbolTable) then begin
            if FMappedUnits.IndexOf(unitSym)<0 then begin
               FMappedUnits.Add(unitSym);
               EnterScope(unitSym);
               MapNormalSymbolNames(unitSym.Table);
               MapNormalSymbolNames(unitSym.ImplementationTable);
               LeaveScope;
            end;
         end;
      end else if (sym is TClassSymbol) or (sym is TRecordSymbol) then begin
         MapStructuredSymbol(TStructuredTypeSymbol(sym), True);
      end else if sym is TInterfaceSymbol then begin
         SymbolMap.MapSymbol(sym, cgssGlobal, True);
      end else if sym is TFuncSymbol then begin
         SymbolMap.MapSymbol(sym, cgssGlobal, True);
         if     (TFuncSymbol(sym).Executable<>nil)
            and (TFuncSymbol(sym).Executable.GetSelf is TdwsProcedure) then
            MapNormalSymbolNames((TFuncSymbol(sym).Executable.GetSelf as TdwsProcedure).Table);
      end else if sym is TDataSymbol then begin
         SymbolMap.MapSymbol(sym, cgssGlobal, True);
      end;
   end;
end;

// CompileProgramBody
//
procedure TdwsCodeGen.CompileProgramBody(expr : TNoResultExpr);
begin
   Compile(expr);
end;

// CompileDependencies
//
procedure TdwsCodeGen.CompileDependencies(destStream : TWriteOnlyBlockStream; const prog : IdwsProgram);
begin
   // nothing
end;

// WriteIndent
//
procedure TdwsCodeGen.WriteIndent;
begin
   Output.WriteString(FIndentString);
end;

// Indent
//
procedure TdwsCodeGen.Indent;
begin
   Inc(FIndent);
   FIndentString:=dwsStringOfChar(' ', FIndent*FIndentSize);
   FNeedIndent:=True;
end;

// UnIndent
//
procedure TdwsCodeGen.UnIndent;
begin
   Dec(FIndent);
   FIndentString:=dwsStringOfChar(' ', FIndent*FIndentSize);
   FNeedIndent:=True;
end;

// WriteString
//
procedure TdwsCodeGen.WriteString(const s : UnicodeString);
begin
   if FNeedIndent then begin
      WriteIndent;
      FNeedIndent:=False;
   end;
   Output.WriteString(s);
end;

// WriteString
//
procedure TdwsCodeGen.WriteString(const c : WideChar);
begin
   if FNeedIndent then begin
      WriteIndent;
      FNeedIndent:=False;
   end;
   Output.WriteChar(c);
end;

// WriteStringLn
//
procedure TdwsCodeGen.WriteStringLn(const s : UnicodeString);
begin
   WriteString(s);
   WriteLineEnd;
end;

// WriteLineEnd
//
procedure TdwsCodeGen.WriteLineEnd;
begin
   Output.WriteString(#13#10);
   FNeedIndent:=True;
end;

// WriteSymbolName
//
procedure TdwsCodeGen.WriteSymbolName(sym : TSymbol; scope : TdwsCodeGenSymbolScope = cgssGlobal);
begin
   WriteString(SymbolMappedName(sym, scope));
end;

// LocationString
//
function TdwsCodeGen.LocationString(e : TExprBase) : UnicodeString;
begin
   if Context is TdwsMainProgram then
      Result:=e.ScriptPos.AsInfo
   else Result:=' in '+e.ScriptLocation(Context);
end;

// GetNewTempSymbol
//
function TdwsCodeGen.GetNewTempSymbol : UnicodeString;
begin
   Inc(FTempSymbolCounter);
   Result:=IntToStr(FTempSymbolCounter);
end;

// WriteCompiledOutput
//
procedure TdwsCodeGen.WriteCompiledOutput(dest : TWriteOnlyBlockStream; const prog : IdwsProgram);
begin
   CompileDependencies(dest, prog);
   dest.WriteString(Output.ToString);
end;

// CompiledOutput
//
function TdwsCodeGen.CompiledOutput(const prog : IdwsProgram) : UnicodeString;
var
   buf : TWriteOnlyBlockStream;
begin
   buf:=TWriteOnlyBlockStream.Create;
   try
      WriteCompiledOutput(buf, prog);
      Result:=buf.ToString;
   finally
      buf.Free;
   end;
end;

// FushDependencies
//
procedure TdwsCodeGen.FushDependencies;
begin
   FFlushedDependencies.Assign(FDependencies);
   FDependencies.Clear;
end;

// EnterContext
//
procedure TdwsCodeGen.EnterContext(proc : TdwsProgram);
  {$IFDEF FPC}
  procedure ProcessExpr(parent, expr : TExprBase; var abort : Boolean);
  var
     i, k, n : Integer;
     sym : TSymbol;
     locName : UnicodeString;
  begin
     if not (expr is TBlockExpr) then Exit;
     for i:=0 to TBlockExpr(expr).Table.Count-1 do begin
        sym:=TBlockExpr(expr).Table.Symbols[i];
        if sym is TDataSymbol then begin
           if FLocalVarSymbolMap.IndexOf(sym.Name)>=0 then begin
              n:=1;
              repeat
                 locName:=dwsFormat('%s_%d', [sym.Name, n]);
                 k:=FLocalVarSymbolMap.IndexOf(locName);
                 Inc(n);
              until k<0;
              FLocalVarSymbolMap.AddObject(locName, sym);
              //FSymbolMap.AddObject(locName, sym);
           end else begin
              FLocalVarSymbolMap.AddObject(sym.Name, sym);
           end;
        end;
     end;

  end;
  {$ENDIF}

var
   i : Integer;
   sym : TSymbol;
begin
   FTableStack.Push(FLocalTable);
   FContextStack.Push(FContext);
   FLocalTable:=proc.Table;
   FContext:=proc;

   FLocalVarSymbolMapStack.Push(FLocalVarSymbolMap);
   FLocalVarSymbolMap:=TStringList.Create;
   for i:=0 to FLocalTable.Count-1 do begin
      sym:=FLocalTable.Symbols[i];
      if sym is TDataSymbol then
         FLocalVarSymbolMap.AddObject(sym.Name, sym);
   end;
   {$IFDEF FPC}
   proc.Expr.RecursiveEnumerateSubExprs(ProcessExpr);
   {$ELSE}
   proc.Expr.RecursiveEnumerateSubExprs(
      procedure (parent, expr : TExprBase; var abort : Boolean)
      var
         i, k, n : Integer;
         sym : TSymbol;
         locName : UnicodeString;
      begin
         if not (expr is TBlockExpr) then Exit;
         for i:=0 to TBlockExpr(expr).Table.Count-1 do begin
            sym:=TBlockExpr(expr).Table.Symbols[i];
            if sym is TDataSymbol then begin
               if FLocalVarSymbolMap.IndexOf(sym.Name)>=0 then begin
                  n:=1;
                  repeat
                     locName:=dwsFormat('%s_%d', [sym.Name, n]);
                     k:=FLocalVarSymbolMap.IndexOf(locName);
                     Inc(n);
                  until k<0;
                  FLocalVarSymbolMap.AddObject(locName, sym);
                  //FSymbolMap.AddObject(locName, sym);
               end else begin
                  FLocalVarSymbolMap.AddObject(sym.Name, sym);
               end;
            end;
         end;
      end);
   {$ENDIF}
end;

// LeaveContext
//
procedure TdwsCodeGen.LeaveContext;
begin
   FLocalTable:=TSymbolTable(FTableStack.Peek);
   FTableStack.Pop;
   FContext:=TdwsProgram(FContextStack.Peek);
   FContextStack.Pop;

   FLocalVarSymbolMap.Free;
   FLocalVarSymbolMap:=TStringList(FLocalVarSymbolMapStack.Peek);
   FLocalVarSymbolMapStack.Pop;
end;

// CreateSymbolMap
//
function TdwsCodeGen.CreateSymbolMap(parentMap : TdwsCodeGenSymbolMap; symbol : TSymbol) : TdwsCodeGenSymbolMap;
begin
   Result:=TdwsCodeGenSymbolMap.Create(FSymbolMap, symbol);
end;

// EnterScope
//
procedure TdwsCodeGen.EnterScope(symbol : TSymbol);
var
   map : TdwsCodeGenSymbolMap;
begin
   if symbol is TUnitSymbol then
      symbol:=TUnitSymbol(symbol).Main;
   map:=FSymbolMaps.MapOf(symbol);
   if map=nil then begin
      FSymbolMap:=CreateSymbolMap(FSymbolMap, symbol);
      FSymbolMap.Maps:=FSymbolMaps;
      FSymbolMaps.Add(FSymbolMap);
   end else begin
      map.FParent:=FSymbolMap;
//      FSymbolMapStack.Push(FSymbolMap);
      FSymbolMap:=map;
   end;
end;

// LeaveScope
//
procedure TdwsCodeGen.LeaveScope;
var
   i : Integer;
   m : TdwsCodeGenSymbolMap;
begin
   Assert(FSymbolMap<>nil);
   m:=FSymbolMap;
   FSymbolMap:=m.Parent;
   m.FParent:=nil;
   if (m.Symbol=nil) or (m.Symbol is TFuncSymbol) then begin
      i:=FSymbolMaps.IndexOf(m);
      FSymbolMaps.Extract(i);
      m.Free;
   end;
//   if (FSymbolMap=nil) and (FSymbolMapStack.Count>0) then begin
//      FSymbolMap:=FSymbolMapStack.Peek;
//      FSymbolMapStack.Pop;
//   end;
end;

// EnterStructScope
//
function TdwsCodeGen.EnterStructScope(struct : TStructuredTypeSymbol) : Integer;
begin
   if struct<>nil then begin
      Result:=EnterStructScope(struct.Parent)+1;
      EnterScope(struct);
   end else Result:=0;
end;

// LeaveScopes
//
procedure TdwsCodeGen.LeaveScopes(n : Integer);
begin
   while n>0 do begin
      LeaveScope;
      Dec(n);
   end;
end;

// IsScopeLevel
//
function TdwsCodeGen.IsScopeLevel(symbol : TSymbol) : Boolean;
var
   m : TdwsCodeGenSymbolMap;
begin
   m:=FSymbolMap;
   while m<>nil do begin
      if m.Symbol=symbol then Exit(True);
      m:=m.Parent;
   end;
   Result:=False;
end;

// RaiseUnknowExpression
//
procedure TdwsCodeGen.RaiseUnknowExpression(expr : TExprBase);
begin
   raise ECodeGenUnknownExpression.CreateFmt('%s: unknown expression class %s:%s',
                                             [ClassName, expr.ClassName, expr.ScriptLocation(Context)]);
end;

// ------------------
// ------------------ TdwsExprGenericCodeGen ------------------
// ------------------

// Create
//
constructor TdwsExprGenericCodeGen.Create(const template : array of const; statement : Boolean = False;
                                          const dependency : UnicodeString = '');
var
   i : Integer;
begin
   inherited Create;
   FStatement:=statement;
   SetLength(FTemplate, Length(template));
   for i:=0 to High(template) do
      FTemplate[i]:=template[i];
   if not FStatement then begin
      i:=High(template);
      FUnWrapable:=    (FTemplate[0].VType=vtWideChar) and (FTemplate[0].VWideChar='(')
                   and (FTemplate[i].VType=vtWideChar) and (FTemplate[i].VWideChar=')');
   end else FUnWrapable:=False;
   FDependency:=dependency;
end;

// CodeGen
//
procedure TdwsExprGenericCodeGen.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
begin
   DoCodeGen(codeGen, expr, 0, High(FTemplate));
end;

// CodeGenNoWrap
//
procedure TdwsExprGenericCodeGen.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
begin
   if FUnWrapable then
      DoCodeGen(codeGen, expr, 1, High(FTemplate)-1)
   else DoCodeGen(codeGen, expr, 0, High(FTemplate));
end;

// DoCodeGen
//
procedure TdwsExprGenericCodeGen.DoCodeGen(codeGen : TdwsCodeGen; expr : TExprBase; start, stop : Integer);
var
   i : Integer;
   c : WideChar;
   item : TExprBase;
begin
   if FDependency<>'' then
      codeGen.Dependencies.Add(FDependency);
   for i:=start to stop do begin
      case FTemplate[i].VType of
         vtInteger : begin
            item:=expr.SubExpr[FTemplate[i].VInteger];
            if     (i>start) and (FTemplate[i-1].VType=vtWideChar) and (FTemplate[i-1].VWideChar='(')
               and (i<stop) and (FTemplate[i+1].VType=vtWideChar) and (FTemplate[i+1].VWideChar=')')
               and (item is TTypedExpr) then begin
               codeGen.CompileNoWrap(TTypedExpr(item));
            end else begin
               codeGen.Compile(item);
            end;
         end;
         vtUnicodeString :
            codeGen.WriteString(UnicodeString(FTemplate[i].VUnicodeString));
         vtWideChar : begin
            c:=FTemplate[i].VWideChar;
            case c of
               #9 : begin
                  codeGen.WriteLineEnd;
                  codeGen.Indent;
               end;
               #8 : codeGen.UnIndent;
            else
               codeGen.WriteString(c);
            end;
         end;
      else
         Assert(False);
      end;
   end;
   if FStatement then
      codeGen.WriteLineEnd;
end;

// ------------------
// ------------------ TdwsExprCodeGen ------------------
// ------------------

// CodeGen
//
procedure TdwsExprCodeGen.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
begin
   if expr is TTypedExpr then begin
      codeGen.WriteString(WideChar('('));
      CodeGenNoWrap(codeGen, TTypedExpr(expr));
      codeGen.WriteString(WideChar(')'));
   end;
end;

// CodeGenNoWrap
//
procedure TdwsExprCodeGen.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
begin
   Self.CodeGen(codeGen, expr);
end;

// ------------------
// ------------------ TdwsMappedSymbolHash ------------------
// ------------------

// SameItem
//
function TdwsMappedSymbolHash.SameItem(const item1, item2 : TdwsMappedSymbol) : Boolean;
begin
   Result:=(item1.Symbol=item2.Symbol);
end;

// GetItemHashCode
//
function TdwsMappedSymbolHash.GetItemHashCode(const item1 : TdwsMappedSymbol) : Integer;
begin
   Result:=NativeInt(item1.Symbol) shr 4;
end;

// ------------------
// ------------------ TdwsCodeGenSymbolMap ------------------
// ------------------

// Create
//
constructor TdwsCodeGenSymbolMap.Create(aParent : TdwsCodeGenSymbolMap; aSymbol : TSymbol);
begin
   inherited Create;
   FHash:=TdwsMappedSymbolHash.Create;
   FParent:=aParent;
   FSymbol:=aSymbol;
   FNames:=TStringList.Create;
   FNames.Sorted:=True;
   FNames.CaseSensitive:=True;
   FNames.Duplicates:=dupError;
   FReservedSymbol:=TSymbol.Create('', nil);
   if aSymbol is TUnitSymbol then
      FPrefix:=aSymbol.Name+'_';
end;

// Destroy
//
destructor TdwsCodeGenSymbolMap.Destroy;
begin
   FReservedSymbol.Free;
   FHash.Free;
   FNames.Free;
   inherited;
end;

// SymbolToName
//
function TdwsCodeGenSymbolMap.SymbolToName(symbol : TSymbol) : UnicodeString;
begin
   FLookup.Symbol:=symbol;
   if FHash.Match(FLookup) then
      Result:=FLookup.Name
   else if Parent<>nil then
      Result:=Parent.SymbolToName(symbol)
   else Result:='';
end;

// ForgetSymbol
//
procedure TdwsCodeGenSymbolMap.ForgetSymbol(symbol : TSymbol);
begin
   FLookup.Symbol:=symbol;
   if not FHash.Extract(FLookup) then
      Assert(False)
   else FNames.Delete(FNames.IndexOfObject(symbol));
end;

// NameToSymbol
//
function TdwsCodeGenSymbolMap.NameToSymbol(const name : UnicodeString; scope : TdwsCodeGenSymbolScope) : TSymbol;
var
   i : Integer;
   iter : TdwsCodeGenSymbolMap;
   skip : Boolean;
   rootMap : TdwsCodeGenSymbolMap;
begin
   i:=FNames.IndexOf(name);
   if i>=0 then
      Result:=TSymbol(FNames.Objects[i])
   else begin
      Result:=nil;
      case scope of
         cgssGlobal : if Parent<>nil then
            Result:=Parent.NameToSymbol(name, scope);
         cgssClass : begin
            if (Parent<>nil) and (Parent.Symbol is TClassSymbol) then
               Result:=Parent.NameToSymbol(name, scope)
            else if Parent<>nil then begin
               // check for root reserved names
               rootMap:=Parent;
               while rootMap.Parent<>nil do
                  rootMap:=rootMap.Parent;
               Result:=rootMap.NameToSymbol(name, cgssLocal);
            end;
         end;
      end;
      if (Result=nil) and (scope=cgssGlobal) then begin
         for i:=0 to Maps.Count-1 do begin
            iter:=Maps[i];
            repeat
               skip:=(iter=Self);
               iter:=iter.Parent;
            until (iter=nil) or skip;
            if not skip then begin
               Result:=Maps[i].NameToSymbol(name, cgssLocal);
               if Result<>nil then Break;
            end;
         end;
      end;
   end;
end;

// ReserveName
//
procedure TdwsCodeGenSymbolMap.ReserveName(const name : UnicodeString);
begin
   FNames.AddObject(name, FReservedSymbol);
end;

// ReserveExternalName
//
procedure TdwsCodeGenSymbolMap.ReserveExternalName(sym : TSymbol);
var
   i : Integer;
begin
   i:=FNames.IndexOf(sym.Name);
   if i<0 then
      FNames.AddObject(sym.Name, sym)
   else begin
      if (FNames.Objects[i]<>FReservedSymbol) and (FNames.Objects[i]<>sym) then
         raise ECodeGenException.CreateFmt('External symbol "%s" already defined', [sym.Name]);
      FNames.Objects[i]:=sym;
   end;
end;

// MapSymbol
//
function TdwsCodeGenSymbolMap.MapSymbol(symbol : TSymbol; scope : TdwsCodeGenSymbolScope; canObfuscate : Boolean) : UnicodeString;
   {
   function NewName : UnicodeString; //workaround for mantis 18702
   var
      i : Integer;
   begin
      i:=0;
      Result:=DoNeedUniqueName(symbol, i, canObfuscate);
      while NameToSymbol(Result, scope)<>nil do begin
         Inc(i);
         Result:=DoNeedUniqueName(symbol, i, canObfuscate);
      end;
      FNames.AddObject(Result, symbol);
      FLookup.Name:=Result;
      FLookup.Symbol:=symbol;
      FHash.Add(FLookup);
   end;
   }
var
   i : Integer;
begin
   Result:=SymbolToName(symbol);
   if Result='' then
   begin
      i:=0;
      Result:=DoNeedUniqueName(symbol, i, canObfuscate);
      while NameToSymbol(Result, scope)<>nil do begin
         Inc(i);
         Result:=DoNeedUniqueName(symbol, i, canObfuscate);
      end;
      FNames.AddObject(Result, symbol);
      FLookup.Name:=Result;
      FLookup.Symbol:=symbol;
      FHash.Add(FLookup);
   end;
end;

// DoNeedUniqueName
//
function TdwsCodeGenSymbolMap.DoNeedUniqueName(symbol : TSymbol; tryCount : Integer; canObfuscate : Boolean) : UnicodeString;
begin
   if symbol.Name='' then begin
      if tryCount=0 then
         Result:='a$'
      else Result:=dwsFormat('a$%d', [tryCount]);
   end else begin;
      if tryCount=0 then
         Result:=Prefix+symbol.Name
      else Result:=dwsFormat('%s%s$%d', [Prefix, symbol.Name, tryCount]);
   end;
end;

// ------------------
// ------------------ TdwsCodeGenSymbolMaps ------------------
// ------------------

// MapOf
//
function TdwsCodeGenSymbolMaps.MapOf(symbol : TSymbol) : TdwsCodeGenSymbolMap;
var
   i : Integer;
begin
   for i:=0 to Count-1 do begin
      Result:=Items[i];
      if Result.Symbol=symbol then Exit;
   end;
   Result:=nil;
end;

end.
