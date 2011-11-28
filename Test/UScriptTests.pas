unit UScriptTests;

interface

uses Classes, SysUtils, dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsUtils,
   dwsXPlatform, dwsSymbols, dws_fpcunit;

type

   TScriptTests = class (TDWSCustomTest)
      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure DoInclude(const scriptName: UnicodeString; var scriptSource: UnicodeString);

         procedure Compilation; override;
         procedure Execution; override;
   end;

   { TScriptFailureTests }

   TScriptFailureTests = class(TDWSCompilerTestCase)
   public
      procedure SetUp; override;
      procedure TearDown; override;

      procedure CompilationFailure(Optimized: boolean);
      procedure DoInclude(const scriptName: UnicodeString; var scriptSource: UnicodeString);
   published
      procedure Simple;
      procedure Optimized;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

procedure EmptyCallBack(parent, expr : TExprBase; var abort : Boolean);
begin
   // just used for detecting crashes in subexpr tree navigation
end;

{ TScriptFailureTests }

procedure TScriptFailureTests.CompilationFailure(Optimized: boolean);
var
   Options: TCompilerOptions;
   ResultExtension: AnsiString;
   ResultFN: AnsiString;
begin
   Options := cDefaultCompilerOptions;
   if Optimized then
   begin
     Options := Options + [coOptimize] - [coAssertions];
     ResultExtension := '.optimized.txt';
     ResultFN := ChangeFileExt(FTestFilename, ResultExtension);
     if FileExists(ResultFN) then
     begin
       FResultFileName := ResultFN;
       FExpectedResult := LoadScriptSource(ResultFN);
     end;
   end
   else begin
     Options := Options -[coOptimize]+[coSymbolDictionary, coContextMap];
   end;
   SetOptions(Options);

   Compile(FSource);

   if FileExists(FResultFileName) then
   begin
      CheckEqualsInfo(FExpectedResult, 'Error messages');
   end
   else
   begin
     Check(fprog.Msgs.AsInfo <> '', 'undetected error');
   end;

   (Fprog as TdwsProgram).InitExpr.RecursiveEnumerateSubExprs(EmptyCallBack);
   (Fprog as TdwsProgram).Expr.RecursiveEnumerateSubExprs(EmptyCallBack);
end;

procedure TScriptFailureTests.DoInclude(const scriptName: UnicodeString;
   var scriptSource: UnicodeString);
begin
   scriptSource := LoadScriptSource('SimpleScripts\'+scriptName);
end;

procedure TScriptFailureTests.Optimized;
begin
  CompilationFailure(True);
end;

procedure TScriptFailureTests.SetUp;
begin
   inherited SetUp;
   FCompiler.OnInclude := DoInclude;
end;

procedure TScriptFailureTests.Simple;
begin
  CompilationFailure(False);
end;

procedure TScriptFailureTests.TearDown;
begin
   inherited TearDown;
end;

// ------------------
// ------------------ TScriptTests ------------------
// ------------------

// SetUp
//
procedure TScriptTests.SetUp;
begin
   SetDecimalSeparator('.');

   FTests:=TStringList.Create;
   FFailures:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'SimpleScripts'+PathDelim, '*.pas', FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'InterfacesPass'+PathDelim, '*.pas', FTests);

   CollectFiles(ExtractFilePath(ParamStr(0))+'FailureScripts'+PathDelim, 'class_missing*.pas', FFailures);
   CollectFiles(ExtractFilePath(ParamStr(0))+'InterfacesFail'+PathDelim, '*.pas', FFailures);

   FCompiler:=TDelphiWebScript.Create(nil);
   inherited;
   FCompiler.OnInclude:=DoInclude;
end;

// TearDown
//
procedure TScriptTests.TearDown;
begin
   inherited;
end;

// DoInclude
//
procedure TScriptTests.DoInclude(const scriptName: UnicodeString; var scriptSource: UnicodeString);
var
   sl : TStringList;
begin
   sl:=TStringList.Create;
   try
      sl.LoadFromFile('SimpleScripts\'+scriptName);
      scriptSource:=sl.Text;
   finally
      sl.Free;
   end;
end;

// Compilation
//
procedure TScriptTests.Compilation;
begin
   Compile(FSource);
   CheckEquals(False, fprog.Msgs.HasErrors, fprog.Msgs.AsInfo);

   (fprog as TdwsProgram).InitExpr.RecursiveEnumerateSubExprs(EmptyCallBack);
   (fprog as TdwsProgram).Expr.RecursiveEnumerateSubExprs(EmptyCallBack);
end;

// Execution
//
procedure TScriptTests.Execution;
var
   output : UnicodeString;
begin
   Compile(FSource);
   CheckEquals(False, fprog.Msgs.HasErrors, fprog.Msgs.AsInfo);
   Execute;

   if fprog.Msgs.Count+fexec.Msgs.Count=0 then
      output:=fexec.Result.ToString
   else begin
      output:= 'Errors >>>>'#13#10
              +fprog.Msgs.AsInfo
              +fexec.Msgs.AsInfo
              +'Result >>>>'#13#10
              +fexec.Result.ToString;
   end;

   CheckEquals(FExpectedResult, output, 'Result');
end;




// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('', TScriptTests.Suite('Scripts', 'SimpleScripts'));
   RegisterTest('', TScriptFailureTests.Suite('FailureScripts', 'FailureScripts'));

end.
