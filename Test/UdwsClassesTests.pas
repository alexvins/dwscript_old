{$I dws_tests_conf.inc}
unit UdwsClassesTests;

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  dws_fpcunit,
  dwsComp, dwsCompiler, dwsExprs,
  dwsClassesLibModule, dwsXPlatform, dwsSymbols;

type

  { TdwsClassesTests }

  TdwsClassesTests = class(TDWSCustomTest)
  private
    FClassesLib: TdwsClassesLib;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  { TSymbolDescriptionsTests }

  TSymbolDescriptionsTests = class(TDWSTestCaseBase)
  private
    FClassesLib: TdwsClassesLib;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure testTstrings;
  end;

 // ------------------------------------------------------------------
 // ------------------------------------------------------------------
 // ------------------------------------------------------------------
implementation

{ TSymbolDescriptionsTests }

procedure TSymbolDescriptionsTests.SetUp;
begin
  inherited SetUp;

  FClassesLib := TdwsClassesLib.Create(nil);
  FClassesLib.Script := FCompiler;

end;

procedure TSymbolDescriptionsTests.TearDown;
begin
  inherited TearDown; //free the compiler first
  FClassesLib.Free;
end;

procedure TSymbolDescriptionsTests.testTstrings;
var
  stringsSymbol: TClassSymbol;
begin
  Compile('');
  stringsSymbol := fprog.Table.FindSymbol('TStrings') as TClassSymbol;
  CheckEquals('property Strings[x: Integer]: String read GetStrings write SetStrings; default;',
    stringsSymbol.Members.FindSymbol('Strings').Description,
    'Strings Description');
end;

{ TdwsClassesTests }

procedure TdwsClassesTests.SetUp;
begin
  inherited;
  FClassesLib := TdwsClassesLib.Create(nil);
  FClassesLib.Script := FCompiler;
end;

procedure TdwsClassesTests.TearDown;
begin
  inherited;
  FClassesLib.Free; //must be freed after compiler
end;


initialization
  RegisterTest('dwsClassesLibTests', TSymbolDescriptionsTests.Suite);
  RegisterTest('', TdwsClassesTests.Suite('ClassesLibTests', 'ClassesLib'));
end.

