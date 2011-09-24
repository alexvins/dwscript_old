unit UdwsFunctionsTests;

interface

uses Classes, SysUtils,
  fpcunit,testregistry,
  dws_fpcunit,
  dwsComp, dwsCompiler, dwsExprs,
  dwsTokenizer, dwsSymbols, dwsMathFunctions, dwsTimeFunctions,
  dwsVariantFunctions;

type

  { TdwsFunctionsTests }

  TdwsFunctionsTests = class (TDWSCustomTest)
  end;


implementation


initialization

   RegisterTest('Functions', TdwsFunctionsTests.Suite('Math','FunctionsMath'));
   RegisterTest('Functions', TdwsFunctionsTests.Suite('Time','FunctionsTime'));
   RegisterTest('Functions', TdwsFunctionsTests.Suite('String','FunctionsString'));
   RegisterTest('Functions', TdwsFunctionsTests.Suite('Variant','FunctionsVariant'));

end.
