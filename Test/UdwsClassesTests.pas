{$I dws_tests_conf.inc}
unit UdwsClassesTests;

interface

uses Classes, SysUtils, fpcunit,testregistry,
   dws_fpcunit,
   dwsComp, dwsCompiler, dwsExprs,
   dwsClassesLibModule, dwsXPlatform, dwsSymbols;

type

   { TdwsClassesTests }

   TdwsClassesTests = class (TDWSCustomTest)
      private
         FClassesLib : TdwsClassesLib;
      public
         procedure SetUp; override;
         procedure TearDown; override;

   end;

  { TSymbolDescriptionsTests }

  TSymbolDescriptionsTests = class(TTestCase)
  private
    FCompiler : TDelphiWebScript;
    FClassesLib : TdwsClassesLib;
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
  FCompiler:=TDelphiWebScript.Create(nil);

  FClassesLib:=TdwsClassesLib.Create(nil);
  FClassesLib.Script:=FCompiler;

end;

procedure TSymbolDescriptionsTests.TearDown;
begin
  FCompiler.Free;
  FClassesLib.Free;
  inherited TearDown;
end;

procedure TSymbolDescriptionsTests.testTstrings;
var
   prog : TdwsProgram;
   stringsSymbol : TClassSymbol;
begin
   prog:=FCompiler.Compile('');
   try
      stringsSymbol:=prog.Table.FindSymbol('TStrings') as TClassSymbol;
      CheckEquals('property Strings[x: Integer]: String read GetStrings write SetStrings; default;',
                  stringsSymbol.Members.FindSymbol('Strings').Description, 'Strings Description');
   finally
      prog.Free;
   end;

end;

{ TdwsClassesTests }

procedure TdwsClassesTests.SetUp;
begin
   inherited;
   FClassesLib:=TdwsClassesLib.Create(nil);
   FClassesLib.Script:=FCompiler;
end;

procedure TdwsClassesTests.TearDown;
begin
   inherited;
   FClassesLib.Free; //must be freed after compiler
end;




initialization

  RegisterTest('dwsClassesLibTests', TSymbolDescriptionsTests.Suite);
  RegisterTest('', TdwsClassesTests.Suite('ClassesLibTests','ClassesLib'));
end.
