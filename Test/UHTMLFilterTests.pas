{$I dws_tests_conf.inc}
unit UHTMLFilterTests;

interface

uses
  Classes, SysUtils,
  fpcunit, testregistry,
  dws_fpcunit,
  dwsComp, dwsCompiler, dwsExprs,
  dwsHtmlFilter, dwsXPlatform;

type

  { THTMLFilterTests }

  THTMLFilterTests = class(TDWSCompilerTestCase)
  private
    FFilter: TdwsHTMLFilter;
    FUnit:   TdwsHTMLUnit;
  protected
    class function GetTestFileMask: string; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure DoInclude(const scriptName: string; var scriptSource: string);

  published
    procedure TestHTMLScript;

  end;

  { TPatternsTests }

  TPatternsTests = class(TTestCase)
  published
    procedure TestPatterns;
  end;

 // ------------------------------------------------------------------
 // ------------------------------------------------------------------
 // ------------------------------------------------------------------
implementation

{ TPatternsTests }

procedure TPatternsTests.TestPatterns;
var
  locFilter: TdwsHtmlFilter;
begin
  locFilter := TdwsHtmlFilter.Create(nil);
  try
    locFilter.PatternClose := '';
    AssertException(EHTMLFilterException, locFilter.CheckPatterns);

    locFilter.PatternClose := 'a';

    locFilter.PatternOpen := '';
    AssertException(EHTMLFilterException, locFilter.CheckPatterns);
    locFilter.PatternOpen := 'b';

    locFilter.PatternEval := '';
    AssertException(EHTMLFilterException, locFilter.CheckPatterns);
    locFilter.PatternEval := 'c';
  finally
    locFilter.Free;
  end;

end;

 // ------------------------------------------------------------------
 // ------------------------------------------------------------------
 // ------------------------------------------------------------------

 // ------------------
 // ------------------ THTMLFilterTests ------------------
 // ------------------

class function THTMLFilterTests.GetTestFileMask: string;
begin
  Result := '*.dws';
end;

// SetUp

procedure THTMLFilterTests.SetUp;
begin
  inherited;
  FCompiler.OnInclude := DoInclude;

  FFilter := TdwsHTMLFilter.Create(nil);
  FFilter.PatternOpen := '<?pas';
  FFilter.PatternClose := '?>';
  FFilter.PatternEval := '=';

  FCompiler.Config.Filter := FFilter;

  FUnit := TdwsHTMLUnit.Create(nil);
  FCompiler.AddUnit(FUnit);
end;

// TearDown

procedure THTMLFilterTests.TearDown;
begin
  FFilter.Free;
  FUnit.Free;
  inherited;
end;

procedure THTMLFilterTests.TestHTMLScript;
begin
  Compilation;
  FProg.Execute;
  CheckEquals(FExpectedResult.Text, (FProg.Result as TdwsDefaultResult).Text,
    'Exec result');
end;


// DoInclude

procedure THTMLFilterTests.DoInclude(const scriptName: string; var scriptSource: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile('SimpleScripts\' + scriptName);
    scriptSource := sl.Text;
  finally
    sl.Free;
  end;
end;

initialization

  RegisterTest('HTMLFilterTests', TPatternsTests.Suite);
  RegisterTest('', THTMLFilterTests.Suite('HTMLFilterTests', 'HTMLFilterScripts'));

end.

