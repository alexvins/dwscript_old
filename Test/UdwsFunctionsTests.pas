unit UdwsFunctionsTests;

interface

uses Classes, SysUtils, dwsXPlatformTests, dwsCompiler,
   dwsTokenizer, dwsSymbols, dwsUtils,
   dwsMathFunctions, dwsTimeFunctions, dwsGlobalVarsFunctions, dwsVariantFunctions,
   dwsMathComplexFunctions,dwsStringFunctions, dwsMath3DFunction,
   dws_fpcunit;

type

   TdwsFunctionsTests = class (TDWSCustomTest)
   end;

   TdwsFuncFunctionsTestsMath = class (TdwsFunctionsTestsBase)
      public
         procedure SetUp; override;
   end;

   TdwsFuncFunctionsTestsMathComplex = class (TdwsFunctionsTestsBase)
      public
         procedure SetUp; override;
   end;

   TdwsFuncFunctionsTestsMath3D = class (TdwsFunctionsTestsBase)
      public
         procedure SetUp; override;
   end;

   TdwsFuncFunctionsTestsTime = class (TdwsFunctionsTestsBase)
      public
         procedure SetUp; override;
   end;

   TdwsFuncFunctionsTestsString = class (TdwsFunctionsTestsBase)
      public
         procedure SetUp; override;
   end;

   TdwsFuncFunctionsTestsVariant = class (TdwsFunctionsTestsBase)
      public
         procedure SetUp; override;
   end;

   TdwsFuncFunctionsTestsGlobalVars = class (TdwsFunctionsTestsBase)
      public
         procedure SetUp; override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------


// SetUp
//
procedure TdwsFunctionsTestsBase.SetUp;
begin
   FormatSettings.DecimalSeparator:='.';

   FCompiler:=TDelphiWebScript.Create(nil);

   FTests:=TStringList.Create;
   CollectFiles(ExtractFilePath(ParamStr(0))+FFolder+PathDelim, '*.pas', FTests);
end;

// TearDown
//
procedure TdwsFunctionsTestsBase.TearDown;
begin
   FTests.Free;

   FCompiler.Free;
end;

// Compilation
//
procedure TdwsFunctionsTestsBase.Compilation;
var
   source : TStringList;
   i : Integer;
   prog : IdwsProgram;
begin
   source:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);
         CheckEquals('', prog.Msgs.AsInfo, FTests[i]);

      end;

   finally
      source.Free;
   end;
end;

// Execution
//
procedure TdwsFunctionsTestsBase.Execution;
var
   source, expectedResult : TStringList;
   i : Integer;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   resultsFileName : String;
begin
   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);
         CheckEquals('', prog.Msgs.AsInfo, FTests[i]);
         exec:=prog.Execute;
         CheckEquals('', exec.Msgs.AsInfo, FTests[i]);
         resultsFileName:=ChangeFileExt(FTests[i], '.txt');
         if FileExists(resultsFileName) then begin
            expectedResult.LoadFromFile(resultsFileName);
            CheckEquals(expectedResult.Text, exec.Result.ToString, FTests[i]);
         end else CheckEquals('', exec.Result.ToString, FTests[i]);

      end;

   finally
      expectedResult.Free;
      source.Free;
   end;
end;

// CompilationNormal
//
procedure TdwsFunctionsTestsBase.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TdwsFunctionsTestsBase.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap, coAssertions];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TdwsFunctionsTestsBase.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coAssertions];
   Execution;
end;

// ExecutionOptimized
//
procedure TdwsFunctionsTestsBase.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize, coAssertions];
   Execution;
end;

// ------------------
// ------------------ TdwsFuncFunctionsTestsMath ------------------
// ------------------

// SetUp
//
procedure TdwsFuncFunctionsTestsMath.SetUp;
begin
   FFolder:='FunctionsMath';
   inherited;
end;

// ------------------
// ------------------ TdwsFuncFunctionsTestsMathComplex ------------------
// ------------------

// SetUp
//
procedure TdwsFuncFunctionsTestsMathComplex.SetUp;
begin
   FFolder:='FunctionsMathComplex';
   inherited;
end;

// ------------------
// ------------------ TdwsFuncFunctionsTestsMath3D ------------------
// ------------------

// SetUp
//
procedure TdwsFuncFunctionsTestsMath3D.SetUp;
begin
   FFolder:='FunctionsMath3D';
   inherited;
end;

// ------------------
// ------------------ TdwsFuncFunctionsTestsTime ------------------
// ------------------

// SetUp
//
procedure TdwsFuncFunctionsTestsTime.SetUp;
begin
   FFolder:='FunctionsTime';
   inherited;
end;

// ------------------
// ------------------ TdwsFuncFunctionsTestsString ------------------
// ------------------

// SetUp
//
procedure TdwsFuncFunctionsTestsString.SetUp;
begin
   FFolder:='FunctionsString';
   inherited;
end;

// ------------------
// ------------------ TdwsFuncFunctionsTestsVariant ------------------
// ------------------

// SetUp
//
procedure TdwsFuncFunctionsTestsVariant.SetUp;
begin
   FFolder:='FunctionsVariant';
   inherited;
end;

// ------------------
// ------------------ TdwsFuncFunctionsTestsGlobalVars ------------------
// ------------------

// SetUp
//
procedure TdwsFuncFunctionsTestsGlobalVars.SetUp;
begin
   FFolder:='FunctionsGlobalVars';
   inherited;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  RegisterTest('Functions', TdwsFunctionsTests.Suite('Math', 'FunctionsMath'));
  RegisterTest('Functions', TdwsFunctionsTests.Suite('MathComplex', 'FunctionsMathComplex'));
  RegisterTest('Functions', TdwsFunctionsTests.Suite('MathComplex3D', 'FunctionsMath3D'));
  RegisterTest('Functions', TdwsFunctionsTests.Suite('Time', 'FunctionsTime'));
  RegisterTest('Functions', TdwsFunctionsTests.Suite('String', 'FunctionsString'));
  RegisterTest('Functions', TdwsFunctionsTests.Suite('Variant', 'FunctionsVariant'));
  RegisterTest('Functions', TdwsFunctionsTests.Suite('GlobalVars', 'FunctionsGlobalVars'));

end.
