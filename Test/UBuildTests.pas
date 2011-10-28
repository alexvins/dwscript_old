unit UBuildTests;

//  Todo
//      Scope implementation to main
//      Scope implementation unit to unit
//      SymbolTable for unit implementation
//      Conditional define locality
//      JS CodeGen of units code

interface

uses Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs, dwsUtils,
   dwsXPlatform, dwsSymbols, dwsFunctions;

type

   TBuildTests = class (TTestCase)
      private
         FTests : TStringList;
         FCompiler : TDelphiWebScript;

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure DoInclude(const scriptName : UnicodeString; var scriptSource : UnicodeString);
         function DoNeedUnit(const unitName : UnicodeString; var unitSource : UnicodeString) : IdwsUnit;

         procedure Compilation;
         procedure Execution;

      published

         procedure CompilationNormal;
         procedure CompilationWithMapAndSymbols;
         procedure ExecutionNonOptimized;
         procedure ExecutionOptimized;
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

// ------------------
// ------------------ TBuildTests ------------------
// ------------------

// SetUp
//
procedure TBuildTests.SetUp;
begin
   SetDecimalSeparator('.');

   FTests:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'BuildScripts'+PathDelim, '*.dws', FTests);

   FCompiler:=TDelphiWebScript.Create(nil);
   FCompiler.OnInclude:=DoInclude;
   FCompiler.OnNeedUnit:=DoNeedUnit;
end;

// TearDown
//
procedure TBuildTests.TearDown;
begin
   FCompiler.Free;

   FTests.Free;
end;

// DoInclude
//
procedure TBuildTests.DoInclude(const scriptName: UnicodeString; var scriptSource: UnicodeString);
var
   sl : TStringList;
   fileName : UnicodeString;
begin
   fileName:='BuildScripts\'+scriptName;
   if FileExists(fileName) then begin
      sl:=TStringList.Create;
      try
         sl.LoadFromFile(fileName);
         scriptSource:=sl.Text;
      finally
         sl.Free;
      end;
   end else scriptSource:='';
end;

// DoNeedUnit
//
function TBuildTests.DoNeedUnit(const unitName : UnicodeString; var unitSource : UnicodeString) : IdwsUnit;
begin
   Result:=nil;
   DoInclude(unitName+'.pas', unitSource);
end;

// Compilation
//
procedure TBuildTests.Compilation;
var
   source, expectedResult : TStringList;
   i : Integer;
   prog : IdwsProgram;
   output, resultsFileName : UnicodeString;
begin
   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);

         if prog.Msgs.HasErrors then begin

            output:= 'Errors >>>>'#13#10
                    +prog.Msgs.AsInfo;

            resultsFileName:=ChangeFileExt(FTests[i], '.txt');
            if FileExists(resultsFileName) then begin
               expectedResult.LoadFromFile(resultsFileName);
               CheckEquals(expectedResult.Text, output, FTests[i]);
            end else CheckEquals('', output, FTests[i]);

         end else begin

            CheckEquals(False, prog.Msgs.HasErrors, FTests[i]+#13#10+prog.Msgs.AsInfo);

            (prog as TdwsProgram).InitExpr.RecursiveEnumerateSubExprs(EmptyCallBack);
            (prog as TdwsProgram).Expr.RecursiveEnumerateSubExprs(EmptyCallBack);

         end;

         prog:=nil;

      end;

   finally
      expectedResult.Free;
      source.Free;
   end;
end;

// Execution
//
procedure TBuildTests.Execution;
var
   source, expectedResult : TStringList;
   i : Integer;
   prog : IdwsProgram;
   resultsFileName : UnicodeString;
   output : UnicodeString;
   exec : IdwsProgramExecution;
begin
   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);

         if not prog.Msgs.HasErrors then begin
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
         end else begin
            output:= 'Errors >>>>'#13#10
                    +prog.Msgs.AsInfo
         end;

         resultsFileName:=ChangeFileExt(FTests[i], '.txt');
         if FileExists(resultsFileName) then begin
            expectedResult.LoadFromFile(resultsFileName);
            CheckEquals(expectedResult.Text, output, FTests[i]);
         end else CheckEquals('', output, FTests[i]);

      end;

   finally
      expectedResult.Free;
      source.Free;
   end;
end;

// CompilationNormal
//
procedure TBuildTests.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TBuildTests.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=cDefaultCompilerOptions+[coSymbolDictionary, coContextMap];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TBuildTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=cDefaultCompilerOptions-[coOptimize];
   Execution;
end;

// ExecutionOptimized
//
procedure TBuildTests.ExecutionOptimized;
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

   TestFramework.RegisterTest('BuildTests', TBuildTests.Suite);

end.
