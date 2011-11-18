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
{                                   }
{**********************************************************************}
unit dwsFunctions;

{$I dws.inc}

interface

uses
  Classes, SysUtils, dwsExprs, dwsSymbols, dwsStack, dwsStrings, dwsTokenizer,
  dwsOperators, dwsUtils, dwsUnitSymbols;

type

   TIdwsUnitFlag = (ufImplicitUse, ufOwnsSymbolTable);
   TIdwsUnitFlags = set of TIdwsUnitFlag;

   // Interface for units
   IdwsUnit = interface
      ['{8D534D12-4C6B-11D5-8DCB-0000216D9E86}']
      function GetUnitName : UnicodeString;
      function GetUnitTable(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                            operators : TOperators) : TUnitSymbolTable;
      function GetDependencies : TStrings;
      function GetUnitFlags : TIdwsUnitFlags;
   end;

   TIdwsUnitList = class(TSimpleList<IdwsUnit>)
      function IndexOfName(const unitName : UnicodeString) : Integer;
      function IndexOf(const aUnit : IdwsUnit) : Integer;
      procedure AddUnits(list : TIdwsUnitList);
   end;

   TEmptyFunc = class(TInterfacedSelfObject, ICallable)
      public
         procedure Call(exec: TdwsProgramExecution; func: TFuncSymbol);
         procedure InitSymbol(Symbol: TSymbol);
         procedure InitExpression(Expr: TExprBase);
   end;

   TFunctionPrototype = class(TInterfacedSelfObject)
      private
         FFuncSymbol : TFuncSymbol;
      public
         procedure InitSymbol(Symbol: TSymbol); virtual;
         procedure InitExpression(Expr: TExprBase); virtual;
         procedure Call(exec: TdwsProgramExecution; func: TFuncSymbol); virtual; abstract;
         property FuncSymbol : TFuncSymbol read FFuncSymbol;
   end;

   TAnonymousFunction = class(TFunctionPrototype, IUnknown, ICallable)
      public
         constructor Create(FuncSym: TFuncSymbol);
         procedure Call(exec: TdwsProgramExecution; func: TFuncSymbol); override;
         procedure Execute(info : TProgramInfo); virtual; abstract;
   end;

   TInternalFunction = class(TFunctionPrototype, IUnknown, ICallable)
      public
         constructor Create(table : TSymbolTable; const funcName : UnicodeString;
                            const params : TParamArray; const funcType : UnicodeString;
                            const isStateLess : Boolean = False); overload; virtual;
         constructor Create(table : TSymbolTable; const funcName : UnicodeString;
                            const params : array of UnicodeString; const funcType : UnicodeString;
                            const isStateLess : Boolean = False); overload;
         procedure Call(exec : TdwsProgramExecution; func : TFuncSymbol); override;
         procedure Execute(info : TProgramInfo); virtual; abstract;
   end;
   TInternalFunctionClass = class of TInternalFunction;

   TAnonymousMethod = class(TFunctionPrototype, IUnknown, ICallable)
      public
         constructor Create(MethSym: TMethodSymbol);
         procedure Call(exec: TdwsProgramExecution; func: TFuncSymbol); override;
         procedure Execute(info : TProgramInfo; var ExternalObject: TObject); virtual; abstract;
   end;

   TInternalMethod = class(TFunctionPrototype, IUnknown, ICallable)
      public
         constructor Create(methKind : TMethodKind; attributes : TMethodAttributes;
                            const methName : UnicodeString; const methParams : array of UnicodeString;
                            const methType : UnicodeString; cls : TClassSymbol;
                            aVisibility : TdwsVisibility;
                            table : TSymbolTable);
         procedure Call(exec : TdwsProgramExecution; func : TFuncSymbol); override;
         procedure Execute(info : TProgramInfo; var externalObject : TObject); virtual; abstract;
   end;

   TInternalInitProc = procedure (systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                                  unitTable : TSymbolTable; operators : TOperators);
   TSymbolsRegistrationProc = procedure (systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                                         unitTable : TSymbolTable);
   TOperatorsRegistrationProc = procedure (systemTable : TSystemSymbolTable; unitTable : TSymbolTable;
                                           operators : TOperators);

   TInternalAbsHandler = function (FProg : TdwsProgram; argExpr : TTypedExpr) : TProgramExpr;
   TInternalSqrHandler = function (FProg : TdwsProgram; argExpr : TTypedExpr) : TProgramExpr;

   TInternalUnit = class(TObject, IdwsUnit)
      private
         FDependencies : TStrings;
         FSymbolsRegistrationProcs : array of TSymbolsRegistrationProc;
         FOperatorsRegistrationProcs : array of TOperatorsRegistrationProc;
         FRegisteredInternalFunctions : TList;
         FStaticSymbols : Boolean;
         FStaticTable : IStaticSymbolTable; // static symbols
         FStaticSystemTable : TSystemSymbolTable;
         FAbsHandlers : array of TInternalAbsHandler;

      protected
         procedure SetStaticSymbols(const Value: Boolean);
         function _AddRef : Integer; stdcall;
         function _Release : Integer; stdcall;
         function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
         function GetDependencies : TStrings;
         function GetUnitName : UnicodeString;

         procedure InitUnitTable(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                                 unitTable : TSymbolTable);
         function GetUnitTable(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                               operators : TOperators) : TUnitSymbolTable;
         function GetUnitFlags : TIdwsUnitFlags;

      public
         constructor Create;
         destructor Destroy; override;

         procedure Lock;
         procedure UnLock;

         procedure AddInternalFunction(rif : Pointer);
         procedure AddSymbolsRegistrationProc(proc : TSymbolsRegistrationProc);
         procedure AddOperatorsRegistrationProc(proc : TOperatorsRegistrationProc);

         procedure AddAbsHandler(const handler : TInternalAbsHandler);
         function HandleAbs(prog : TdwsProgram; argExpr : TTypedExpr) : TProgramExpr;

         procedure InitStaticSymbols(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                                     operators : TOperators);
         procedure ReleaseStaticSymbols;

         property StaticTable : IStaticSymbolTable read FStaticTable;
         property StaticSymbols : Boolean read FStaticSymbols write SetStaticSymbols;
   end;

   TSourceUnit = class(TInterfacedObject, IdwsUnit)
      private
         FDependencies : TStrings;
         FSymbol : TUnitMainSymbol;

      protected

      public
         constructor Create(const unitName : UnicodeString; rootTable : TSymbolTable;
                            unitSyms : TUnitMainSymbols);
         destructor Destroy; override;

         function GetUnitName : UnicodeString;
         function GetUnitTable(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                               operators : TOperators) : TUnitSymbolTable;
         function GetDependencies : TStrings;
         function GetUnitFlags : TIdwsUnitFlags;

         property Symbol : TUnitMainSymbol read FSymbol write FSymbol;
   end;

procedure RegisterInternalFunction(InternalFunctionClass: TInternalFunctionClass;
      const FuncName: UnicodeString; const FuncParams: array of UnicodeString;
      const FuncType: UnicodeString; const isStateLess : Boolean = False);
procedure RegisterInternalProcedure(InternalFunctionClass: TInternalFunctionClass;
      const FuncName: UnicodeString; const FuncParams: array of UnicodeString);

procedure RegisterInternalSymbolsProc(proc : TSymbolsRegistrationProc);
procedure RegisterInternalOperatorsProc(proc : TOperatorsRegistrationProc);

function dwsInternalUnit : TInternalUnit;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
   vInternalUnit : TInternalUnit;

// dwsInternalUnit
//
function dwsInternalUnit : TInternalUnit;
begin
   if not Assigned(vInternalUnit) then
      vInternalUnit:=TInternalUnit.Create;
   Result:=vInternalUnit;
end;

// RegisterInternalSymbolsProc
//
procedure RegisterInternalSymbolsProc(proc : TSymbolsRegistrationProc);
begin
   dwsInternalUnit.AddSymbolsRegistrationProc(proc);
end;

// RegisterInternalOperatorsProc
//
procedure RegisterInternalOperatorsProc(proc : TOperatorsRegistrationProc);
begin
   dwsInternalUnit.AddOperatorsRegistrationProc(proc);
end;

// ConvertFuncParams
//
function ConvertFuncParams(const funcParams : array of UnicodeString) : TParamArray;

   procedure ParamSpecifier(c : WideChar; paramRec : PParamRec);
   begin
      paramRec.IsVarParam:=(c='@');
      paramRec.IsConstParam:=(c='&');
      paramRec.ParamName:=Copy(paramRec.ParamName, 2, MaxInt)
   end;

   procedure ParamDefaultValue(p : Integer; paramRec : PParamRec);
   begin
      SetLength(paramRec.DefaultValue, 1);
      paramRec.DefaultValue[0]:=Trim(Copy(paramRec.ParamName, p+1, MaxInt));
      paramRec.HasDefaultValue:=True;
      paramRec.ParamName:=Trim(Copy(paramRec.ParamName, 1, p-1));
   end;

var
   x, p : Integer;
   c : WideChar;
   paramRec : PParamRec;
begin
   SetLength(Result, Length(funcParams) div 2);
   x:=0;
   while x<Length(funcParams)-1 do begin
      paramRec:=@Result[x div 2];

      paramRec.ParamName:=funcParams[x];
      c:=#0;
      if paramRec.ParamName<>'' then
         c:=paramRec.ParamName[1];

      case c of
         '@','&':
            ParamSpecifier(c, paramRec);
      else
         paramRec.IsVarParam:=False;
         paramRec.IsConstParam:=False;
      end;

      p:=Pos('=', paramRec.ParamName);
      if p>0 then
         ParamDefaultValue(p, paramRec);

      paramRec.ParamType:=funcParams[x+1];

      Inc(x, 2);
   end;
end;

type
   TRegisteredInternalFunction = record
      InternalFunctionClass : TInternalFunctionClass;
      FuncName : UnicodeString;
      FuncParams : TParamArray;
      FuncType : UnicodeString;
      StateLess : Boolean;
   end;
   PRegisteredInternalFunction = ^TRegisteredInternalFunction;

// RegisterInternalFunction
//
procedure RegisterInternalFunction(InternalFunctionClass: TInternalFunctionClass;
                                   const FuncName: UnicodeString;
                                   const FuncParams: array of UnicodeString;
                                   const FuncType: UnicodeString;
                                   const isStateLess : Boolean = False);
var
   rif : PRegisteredInternalFunction;
begin
   New(rif);
   rif.InternalFunctionClass := InternalFunctionClass;
   rif.FuncName := FuncName;
   rif.StateLess:=isStateLess;
   rif.FuncParams:=ConvertFuncParams(FuncParams);
   rif.FuncType := FuncType;

   dwsInternalUnit.AddInternalFunction(rif);
end;

// RegisterInternalProcedure
//
procedure RegisterInternalProcedure(InternalFunctionClass: TInternalFunctionClass;
      const FuncName: UnicodeString; const FuncParams: array of UnicodeString);
begin
   RegisterInternalFunction(InternalFunctionClass, FuncName, FuncParams, '', False);
end;

{ TEmptyFunc }

procedure TEmptyFunc.Call(exec: TdwsProgramExecution; func: TFuncSymbol);
begin
end;

procedure TEmptyFunc.InitSymbol(Symbol: TSymbol);
begin
end;

procedure TEmptyFunc.InitExpression(Expr: TExprBase);
begin
end;

{ TFunctionPrototype }

procedure TFunctionPrototype.InitSymbol(Symbol: TSymbol);
begin
end;

procedure TFunctionPrototype.InitExpression(Expr: TExprBase);
begin
end;

// ------------------
// ------------------ TInternalFunction ------------------
// ------------------

constructor TInternalFunction.Create(table : TSymbolTable; const funcName : UnicodeString;
                                     const params : TParamArray; const funcType : UnicodeString;
                                     const isStateLess : Boolean = False);
var
   sym: TFuncSymbol;
begin
   sym:=TFuncSymbol.Generate(table, funcName, params, funcType);
   sym.Params.AddParent(table);
   sym.Executable:=ICallable(Self);
   sym.IsStateless:=isStateLess;
   FFuncSymbol:=sym;
   table.AddSymbol(sym);
end;

// Create
//
constructor TInternalFunction.Create(table: TSymbolTable; const funcName : UnicodeString;
                                     const params : array of UnicodeString; const funcType : UnicodeString;
                                     const isStateLess : Boolean = False);
begin
   Create(table, funcName, ConvertFuncParams(params), funcType, isStateLess);
end;

// Call
//
procedure TInternalFunction.Call(exec : TdwsProgramExecution; func : TFuncSymbol);
var
   info : TProgramInfo;
begin
   info:=exec.AcquireProgramInfo(func);
   try
      Execute(info);
   finally
      exec.ReleaseProgramInfo(info);
   end;
end;

// ------------------
// ------------------ TInternalMethod ------------------
// ------------------

// Create
//
constructor TInternalMethod.Create(methKind: TMethodKind; attributes: TMethodAttributes;
                                   const methName: UnicodeString; const methParams: array of UnicodeString;
                                   const methType: UnicodeString; cls: TClassSymbol;
                                   aVisibility : TdwsVisibility;
                                   table: TSymbolTable);
var
   sym : TMethodSymbol;
   params : TParamArray;
begin
   params:=ConvertFuncParams(methParams);

   sym:=TMethodSymbol.Generate(table, methKind, attributes, methName, Params,
                                 methType, cls, aVisibility);
   sym.Params.AddParent(table);
   sym.Executable := ICallable(Self);

   // Add method to its class
   cls.AddMethod(sym);
end;

procedure TInternalMethod.Call(exec: TdwsProgramExecution; func: TFuncSymbol);
var
   scriptObj: IScriptObj;
   extObj: TObject;
   info : TProgramInfo;
begin
   info:=exec.AcquireProgramInfo(func);
   try
      scriptObj := Info.Vars[SYS_SELF].ScriptObj;

      if Assigned(scriptObj) then begin
         info.ScriptObj := scriptObj;
         extObj := scriptObj.ExternalObject;
         try
            Execute(info, extObj);
         finally
            scriptObj.ExternalObject := extObj;
            info.ScriptObj := nil;
         end;
      end else begin
         // Class methods or method calls on nil-object-references
         extObj := nil;
         Execute(info, extObj);
      end;
   finally
      exec.ReleaseProgramInfo(info);
   end;
end;

{ TAnonymousFunction }

constructor TAnonymousFunction.Create(FuncSym: TFuncSymbol);
begin
   FuncSym.Executable := ICallable(Self);
end;

// Call
//
procedure TAnonymousFunction.Call(exec: TdwsProgramExecution; func: TFuncSymbol);
var
   info : TProgramInfo;
begin
   info:=exec.AcquireProgramInfo(func);
   try
      Execute(info);
   finally
      exec.ReleaseProgramInfo(info);
   end;
end;

{ TAnonymousMethod }

constructor TAnonymousMethod.Create(MethSym: TMethodSymbol);
begin
   MethSym.Executable := ICallable(Self);
end;

procedure TAnonymousMethod.Call(exec: TdwsProgramExecution; func: TFuncSymbol);
var
   info : TProgramInfo;
   scriptObj : IScriptObj;
   extObj : TObject;
begin
   info:=exec.AcquireProgramInfo(func);
   try
      scriptObj:=info.Vars[SYS_SELF].ScriptObj;

      if Assigned(scriptObj) then begin
         info.ScriptObj := scriptObj;
         extObj := scriptObj.ExternalObject;
         try
            Execute(info, extObj);
         finally
            scriptObj.ExternalObject := extObj;
         end;
      end else begin
         // Class methods or method calls on nil-object-references
         extObj := nil;
         Execute(info, extObj);
      end;
   finally
      exec.ReleaseProgramInfo(info);
   end;
end;

// ------------------
// ------------------ TInternalUnit ------------------
// ------------------

// Create
//
constructor TInternalUnit.Create;
begin
   FDependencies:=TStringList.Create;
   FRegisteredInternalFunctions:=TList.Create;
   FStaticSymbols:=True;
end;

// Destroy
//
destructor TInternalUnit.Destroy;
var
   i : Integer;
   rif : PRegisteredInternalFunction;
begin
   ReleaseStaticSymbols;
   FDependencies.Free;
   for i:=0 to FRegisteredInternalFunctions.Count - 1 do begin
      rif:=PRegisteredInternalFunction(FRegisteredInternalFunctions[i]);
      Dispose(rif);
   end;
   FRegisteredInternalFunctions.Free;
   inherited;
end;

// Lock
//
procedure TInternalUnit.Lock;
begin
   TMonitor.Enter(Self);
end;

// UnLock
//
procedure TInternalUnit.UnLock;
begin
   TMonitor.Exit(Self);
end;

// AddSymbolsRegistrationProc
//
procedure TInternalUnit.AddSymbolsRegistrationProc(proc : TSymbolsRegistrationProc);
var
   n : Integer;
begin
   n:=Length(FSymbolsRegistrationProcs);
   SetLength(FSymbolsRegistrationProcs, n+1);
   FSymbolsRegistrationProcs[n]:=proc;
end;

// AddOperatorsRegistrationProc
//
procedure TInternalUnit.AddOperatorsRegistrationProc(proc : TOperatorsRegistrationProc);
var
   n : Integer;
begin
   n:=Length(FOperatorsRegistrationProcs);
   SetLength(FOperatorsRegistrationProcs, n+1);
   FOperatorsRegistrationProcs[n]:=proc;
end;

// AddAbsHandler
//
procedure TInternalUnit.AddAbsHandler(const handler : TInternalAbsHandler);
var
   n : Integer;
begin
   n:=Length(FAbsHandlers);
   SetLength(FAbsHandlers, n+1);
   FAbsHandlers[n]:=handler;
end;

// HandleAbs
//
function TInternalUnit.HandleAbs(prog : TdwsProgram; argExpr : TTypedExpr) : TProgramExpr;
var
   i : Integer;
begin
   Result:=nil;
   for i:=0 to High(FAbsHandlers) do begin
      Result:=FAbsHandlers[i](prog, argExpr);
      if Result<>nil then Break;
   end;
end;

// AddInternalFunction
//
procedure TInternalUnit.AddInternalFunction(rif: Pointer);
begin
   FRegisteredInternalFunctions.Add(rif);
end;

// _AddRef
//
function TInternalUnit._AddRef : Integer;
begin
   Result:=-1;
end;

// GetDependencies
//
function TInternalUnit.GetDependencies: TStrings;
begin
   Result:=FDependencies;
end;

// GetUnitName
//
function TInternalUnit.GetUnitName : UnicodeString;
begin
   Result:=SYS_INTERNAL;
end;

// InitStaticSymbols
//
procedure TInternalUnit.InitStaticSymbols(
   systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols; operators : TOperators);
begin
   if FStaticTable=nil then begin
      FStaticSystemTable:=systemTable;
      FStaticTable:=TStaticSymbolTable.Create(systemTable);
      try
         InitUnitTable(systemTable, unitSyms, FStaticTable.SymbolTable);
      except
         ReleaseStaticSymbols;
         raise;
      end;
   end;
end;

// ReleaseStaticSymbols
//
procedure TInternalUnit.ReleaseStaticSymbols;
begin
   FStaticSystemTable:=nil;
   FStaticTable:=nil;
end;

// GetUnitTable
//
function TInternalUnit.GetUnitTable(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                                    operators : TOperators) : TUnitSymbolTable;
var
   i : Integer;
begin
   Lock;
   try
      if FStaticSystemTable<>systemTable then
         ReleaseStaticSymbols;

      if StaticSymbols then begin
         InitStaticSymbols(systemTable, unitSyms, operators);
         Result:=TLinkedSymbolTable.Create(FStaticTable.SymbolTable);
      end else begin
         Result:=TUnitSymbolTable.Create(SystemTable);
         try
            InitUnitTable(systemTable, unitSyms, Result);
         except
            Result.Free;
            raise;
         end;
      end;

      for i:=0 to High(FOperatorsRegistrationProcs) do
         FOperatorsRegistrationProcs[i](systemTable, Result, operators);
   finally
      UnLock;
   end;
end;

// GetUnitFlags
//
function TInternalUnit.GetUnitFlags : TIdwsUnitFlags;
begin
   Result:=[ufImplicitUse];
end;

// InitUnitTable
//
procedure TInternalUnit.InitUnitTable(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                                      unitTable : TSymbolTable);
var
   i : Integer;
   rif : PRegisteredInternalFunction;
begin
   for i := 0 to High(FSymbolsRegistrationProcs) do
      FSymbolsRegistrationProcs[i](systemTable, unitSyms, unitTable);

   for i := 0 to FRegisteredInternalFunctions.Count - 1 do begin
      rif := PRegisteredInternalFunction(FRegisteredInternalFunctions[i]);
      try
         rif.InternalFunctionClass.Create(unitTable, rif.FuncName, rif.FuncParams,
                                          rif.FuncType, rif.StateLess);
      except
         on e: Exception do
            raise Exception.CreateFmt('AddInternalFunctions failed on %s'#13#10'%s',
                                      [rif.FuncName, e.Message]);
      end;
   end;
end;

function TInternalUnit.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := 0;
end;

function TInternalUnit._Release: Integer;
begin
  Result := -1;
end;

procedure TInternalUnit.SetStaticSymbols(const Value: Boolean);
begin
  FStaticSymbols := Value;
  if not FStaticSymbols then
    ReleaseStaticSymbols;
end;

// ------------------
// ------------------ TIdwsUnitList ------------------
// ------------------

// IndexOf (name)
//
function TIdwsUnitList.IndexOfName(const unitName : UnicodeString) : Integer;
begin
   for Result:=0 to Count-1 do
      if SameText(Items[Result].GetUnitName, unitName) then
         Exit;
   Result:=-1;
end;

// AddUnits
//
procedure TIdwsUnitList.AddUnits(list : TIdwsUnitList);
var
   i : Integer;
begin
   for i:=0 to list.Count-1 do
      Add(list[i]);
end;

// IndexOf (IdwsUnit)
//
function TIdwsUnitList.IndexOf(const aUnit : IdwsUnit) : Integer;
begin
   for Result:=0 to Count-1 do
      if Items[Result]=aUnit then
         Exit;
   Result:=-1;
end;


// ------------------
// ------------------ TSourceUnit ------------------
// ------------------

// Create
//
constructor TSourceUnit.Create(const unitName : UnicodeString; rootTable : TSymbolTable;
                               unitSyms : TUnitMainSymbols);
var
   ums : TUnitMainSymbol;
begin
   inherited Create;
   FDependencies:=TStringList.Create;
   FSymbol:=TUnitMainSymbol.Create(unitName, TUnitSymbolTable.Create(nil, rootTable.AddrGenerator), unitSyms);
   FSymbol.ReferenceInSymbolTable(rootTable);

   FSymbol.CreateInterfaceTable;

   unitSyms.Find(SYS_SYSTEM).ReferenceInSymbolTable(FSymbol.InterfaceTable);
   unitSyms.Find(SYS_INTERNAL).ReferenceInSymbolTable(FSymbol.InterfaceTable);

   ums:=unitSyms.Find(SYS_DEFAULT);
   if ums<>nil then
      ums.ReferenceInSymbolTable(FSymbol.InterfaceTable);
end;

// Destroy
//
destructor TSourceUnit.Destroy;
begin
   FDependencies.Free;
   inherited;
end;

// GetUnitName
//
function TSourceUnit.GetUnitName : UnicodeString;
begin
   Result:=Symbol.Name;
end;

// GetUnitTable
//
function TSourceUnit.GetUnitTable(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                                  operators : TOperators) : TUnitSymbolTable;
begin
   Result:=(Symbol.Table as TUnitSymbolTable);
end;

// GetDependencies
//
function TSourceUnit.GetDependencies : TStrings;
begin
   Result:=FDependencies;
end;

// GetUnitFlags
//
function TSourceUnit.GetUnitFlags : TIdwsUnitFlags;
begin
   Result:=[ufOwnsSymbolTable];
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

finalization

   FreeAndNil(vInternalUnit);

end.
