{$I dws_tests_conf.inc}
unit UMemoryTests;

interface

uses Classes, SysUtils,
  fpcunit, testregistry,
  dws_fpcunit,
  dwsComp, dwsCompiler, dwsExprs,
  {dwsComConnector,} Variants, {ActiveX, ComObj,} dwsXPlatform;

type

  { TMemoryTests }

  TMemoryTests = class(TDWSCustomTest)
  protected
    procedure PostExec; override;
  end;

 // ------------------------------------------------------------------
 // ------------------------------------------------------------------
 // ------------------------------------------------------------------
implementation

{ TMemoryTests }

procedure TMemoryTests.PostExec;
begin
  inherited PostExec;
  CheckEquals(0, FProg.ObjectCount, 'Leaked ' + IntToStr(FProg.ObjectCount));
end;


 // ------------------------------------------------------------------
 // ------------------------------------------------------------------
 // ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  RegisterTest('', TMemoryTests.Suite('MemoryTests', 'Memory'));

end.

