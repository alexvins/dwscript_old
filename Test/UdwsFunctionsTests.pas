unit UdwsFunctionsTests;

interface

uses Classes, SysUtils, {dwsXPlatformTests,} dwsComp, dwsCompiler, dwsExprs,
   dwsTokenizer, dwsSymbols, dwsXPlatform, dwsUtils,
   dwsMathFunctions, dwsTimeFunctions, dwsGlobalVarsFunctions, dwsVariantFunctions,
   dwsMathComplexFunctions,
   dws_fpcunit,fpcunit, testregistry;

type

   TdwsFunctionsTests = class (TDWSCustomTest)
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  RegisterTest('Functions', TdwsFunctionsTests.Suite('Math', 'FunctionsMath'));
  RegisterTest('Functions', TdwsFunctionsTests.Suite('MathComplex', 'FunctionsMathComplex'));
  RegisterTest('Functions', TdwsFunctionsTests.Suite('Time', 'FunctionsTime'));
  RegisterTest('Functions', TdwsFunctionsTests.Suite('String', 'FunctionsString'));
  RegisterTest('Functions', TdwsFunctionsTests.Suite('Variant', 'FunctionsVariant'));
  RegisterTest('Functions', TdwsFunctionsTests.Suite('GlobalVars', 'FunctionsGlobalVars'));

end.
