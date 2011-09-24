unit UAlgorithmsTests;

interface

uses Classes, SysUtils,
  fpcunit,testregistry,
  dws_fpcunit,
  dwsComp, dwsCompiler, dwsExprs, dwsXPlatform;

type

  { TAlgorithmsTests }

  TAlgorithmsTests = class (TDWSCustomTest)
  end;

implementation



initialization

   RegisterTest('',TAlgorithmsTests.Suite('AlgorithmsTests','Algorithms'));

end.
