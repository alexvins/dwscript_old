{$I dws_tests_conf.inc}
unit UAlgorithmsTests;

interface

uses testregistry,
  dws_fpcunit;

type

  { TAlgorithmsTests }

  TAlgorithmsTests = class(TDWSCustomTest)
  end;

implementation


initialization

  RegisterTest('', TAlgorithmsTests.Suite('AlgorithmsTests', 'Algorithms'));

end.

