unit dws_fpcunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TDWSTestFileTest }

  TDWSTestFileTest = class(TTestCase)
  protected
    procedure SetUp; override; 
    procedure TearDown; override;
    procedure RunTest; override;
  end;

  { TTestFactory }

  TTestFactory = class

  end;

implementation

procedure TDWSTestFileTest.SetUp;
begin

end; 

procedure TDWSTestFileTest.TearDown;
begin

end;

procedure TDWSTestFileTest.RunTest;
begin

end;

end.

