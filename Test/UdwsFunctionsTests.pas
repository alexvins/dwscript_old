unit UdwsFunctionsTests;

interface

uses Classes, SysUtils, dwsXPlatformTests, dwsCompiler,
   dwsTokenizer, dwsSymbols, dwsUtils,
   dwsMathFunctions, dwsTimeFunctions, dwsGlobalVarsFunctions, dwsVariantFunctions,
   dwsMathComplexFunctions,dwsStringFunctions, dwsMath3DFunctions,
   dws_fpcunit;

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
  RegisterTest('Functions', TdwsFunctionsTests.Suite('MathComplex3D', 'FunctionsMath3D'));
  RegisterTest('Functions', TdwsFunctionsTests.Suite('Time', 'FunctionsTime'));
  RegisterTest('Functions', TdwsFunctionsTests.Suite('String', 'FunctionsString'));
  RegisterTest('Functions', TdwsFunctionsTests.Suite('Variant', 'FunctionsVariant'));
  RegisterTest('Functions', TdwsFunctionsTests.Suite('GlobalVars', 'FunctionsGlobalVars'));

end.
