unit dws_fpcunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  dwsComp, dwsCompiler, dwsXPlatform;

type

  { TDWSCompilerTestCase }

  TDWSCompilerTestCase = class abstract(TTestCase)
  strict protected
    FCompiler: TDelphiWebScript;
    FTestFilename: string;
  protected
    property TestFilename:string read FTestFilename;
    procedure SetUp; override; 
    procedure TearDown; override;

    class function GetTestDataPath: string; virtual; abstract;
    class function GetTestFileMask: string; virtual;
  public
    constructor CreateWith(const ATestName: string;
      const ATestSuiteName: string; const ATestFile: String); reintroduce;virtual;
    class function Suite: TTest;
  end;

  TDWSCompilerTestCaseClass = class of TDWSCompilerTestCase;

  { TTestFactory }

  TTestFactory = class (TTestSuite)
  public
    constructor Create(AClass: TDWSCompilerTestCaseClass); reintroduce;
  end;

  { TTestFileSuite }

  TTestFileSuite = class(TTestSuite)
  public
    constructor Create(ATestFile: string; ATestClass: TDWSCompilerTestCaseClass);reintroduce;

  end;

implementation

{ TTestFileSuite }

constructor TTestFileSuite.Create(ATestFile: string;
  ATestClass: TDWSCompilerTestCaseClass);
var
  ml: TStringList;
  s: String;
  the_test: TDWSCompilerTestCase;
begin
  inherited Create(ExtractFileName(ATestFile));
  ml := TStringList.Create;
  try
    GetMethodList(ATestClass, ml);
    for s in ml do
    begin
      the_test := ATestClass.CreateWith(s, ExtractFileName(ATestFile), ATestFile);

      AddTest(the_test);
    end;
  finally
    ml.Free;
  end
end;

{ TTestFactory }

constructor TTestFactory.Create(AClass: TDWSCompilerTestCaseClass);
var
  test_data_path: string;
  FFiles: TStringList;
  s: String;
  file_suite: TTestFileSuite;
begin
  inherited Create(AClass.ClassName);
  //collect tests
  test_data_path := ExtractFilePath(ParamStr(0))
    +AClass.GetTestDataPath+DirectorySeparator;
  FFiles := TStringList.Create;
  try
    try
      CollectFiles(test_data_path, AClass.GetTestFileMask, FFiles);
      for s in FFiles do
      begin
        file_suite := TTestFileSuite.Create(s,AClass);
        AddTest(file_suite);
      end;
    finally
      FFiles.Free;
    end;
  except
    AddTest(Warning('Exception during test creation'));
  end;
end;

procedure TDWSCompilerTestCase.SetUp;
begin
  inherited;
  FCompiler := TDelphiWebScript.Create(nil);
end; 

procedure TDWSCompilerTestCase.TearDown;
begin
  FCompiler.Free;
  inherited;
end;

class function TDWSCompilerTestCase.GetTestFileMask: string;
begin
  Result := '*.pas';
end;

constructor TDWSCompilerTestCase.CreateWith(const ATestName: string;
  const ATestSuiteName: string; const ATestFile: String);
begin
  inherited CreateWith(ATestName,ATestSuiteName);
  FTestFilename := ATestFile;
end;

class function TDWSCompilerTestCase.Suite: TTest;
begin
  Result := TTestFactory.Create(Self);
end;

end.

