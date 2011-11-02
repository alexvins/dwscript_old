unit UScriptTests;

interface

uses Classes, SysUtils, dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsUtils,
   dwsXPlatform, dwsSymbols, dws_fpcunit;

type

   TScriptTests = class (TTestCase)
      private
         FTests : TStringList;
         FFailures : TStringList;
         FCompiler : TDelphiWebScript;

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure DoInclude(const scriptName: UnicodeString; var scriptSource: UnicodeString);

         procedure Compilation;
         procedure Execution;
         procedure CompilationFailure;

      published

         procedure CompilationNormal;
         procedure CompilationWithMapAndSymbols;
         procedure ExecutionNonOptimized;
         procedure ExecutionOptimized;
         procedure FailuresNonOptimized;
         procedure FailuresOptimized;
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

   CollectFiles(ExtractFilePath(ParamStr(0))+'FailureScripts'+PathDelim, '*.pas', FFailures);
   CollectFiles(ExtractFilePath(ParamStr(0))+'InterfacesFail'+PathDelim, '*.pas', FFailures);

   FCompiler:=TDelphiWebScript.Create(nil);
   FCompiler.OnInclude:=DoInclude;
end;

// TearDown
//
procedure TScriptTests.TearDown;
begin
   FCompiler.Free;

   FTests.Free;
   FFailures.Free;
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
var
   i : Integer;
   prog : IdwsProgram;
begin

   for i:=0 to FTests.Count-1 do begin

      prog:=FCompiler.Compile(LoadScriptSource(FTests[i]));

      CheckEquals(False, prog.Msgs.HasErrors, FTests[i]+#13#10+prog.Msgs.AsInfo);

      (prog as TdwsProgram).InitExpr.RecursiveEnumerateSubExprs(EmptyCallBack);
      (prog as TdwsProgram).Expr.RecursiveEnumerateSubExprs(EmptyCallBack);

   end;

end;

// Execution
//
procedure TScriptTests.Execution;
var
   expectedResult : TStringList;
   i : Integer;
   prog : IdwsProgram;
   resultsFileName : UnicodeString;
   output : UnicodeString;
   exec : IdwsProgramExecution;
begin
   expectedResult:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         prog:=FCompiler.Compile(LoadScriptSource(FTests[i]));

         CheckEquals(False, prog.Msgs.HasErrors, FTests[i]+#13#10+prog.Msgs.AsInfo);
         try
            exec:=prog.Execute;
         except
            on E: Exception do begin
               CheckEquals('', E.Message, FTests[i]);
            end;
         end;
         if prog.Msgs.Count+exec.Msgs.Count=0 then
            output:=exec.Result.ToString
         else begin
            output:= 'Errors >>>>'#13#10
                    +prog.Msgs.AsInfo
                    +exec.Msgs.AsInfo
                    +'Result >>>>'#13#10
                    +exec.Result.ToString;
         end;
         resultsFileName:=ChangeFileExt(FTests[i], '.txt');
         if FileExists(resultsFileName) then begin
            expectedResult.LoadFromFile(resultsFileName);
            CheckEquals(expectedResult.Text, output, FTests[i]);
         end else CheckEquals('', output, FTests[i]);

      end;

   finally
      expectedResult.Free;
   end;
end;

// CompilationNormal
//
procedure TScriptTests.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TScriptTests.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=cDefaultCompilerOptions+[coSymbolDictionary, coContextMap];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TScriptTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=cDefaultCompilerOptions-[coOptimize];
   Execution;
end;

// ExecutionOptimized
//
procedure TScriptTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=cDefaultCompilerOptions+[coOptimize];
   Execution;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('ScriptTests', TScriptTests);
   RegisterTest('', TScriptFailureTests.Suite('FailureScripts', 'FailureScripts'));

end.
