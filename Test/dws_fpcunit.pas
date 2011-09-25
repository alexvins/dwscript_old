unit dws_fpcunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  dwsComp, dwsCompiler, dwsExprs, dwsXPlatform;

type

  { TDWSTestCaseBase }

  TDWSTestCaseBase = class(TTestCase)
  strict protected
    FCompiler: TDelphiWebScript;
    FProg:     TdwsProgram;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    procedure SetOptions(const AOptions: TCompilerOptions);
    procedure Compile(const ASource: string);
    procedure Compile(const ASource: TStrings);
    procedure Execute;

    procedure CheckEqualsInfo(Expected: string; msg: string = '');
    procedure CheckEmptyInfo(msg: string = '');
    procedure CheckEqualsResult(Expected: string; msg: string = '');
    //default result as text

  end;

  { TDWSCompilerTestCase }

  TDWSCompilerTestCase = class(TDWSTestCaseBase)
  private
    FOldDS: Char;
  strict protected
    FTestFilename: string;

    FSource: TStringList;


    FResultFileName: string;
    FExpectedResult: TStringList;

  protected
    property TestFilename: string read FTestFilename;
    procedure SetUp; override;
    procedure TearDown; override;

    procedure Compilation; virtual;
    procedure Execution; virtual;


    procedure PostExec; virtual;

    class function GetTestFileMask: string; virtual;


  public
    constructor CreateWith(const ATestName: string; const ATestSuiteName: string;
      const ATestFile: string); reintroduce; virtual;
    class function Suite(const AName, AFolder: string): TTest;
  public
    procedure CompilationNormal;
    procedure CompilationWithMapAndSymbols;
    procedure ExecutionNonOptimized;
    procedure ExecutionOptimized;
  end;

  TDWSCompilerTestCaseClass = class of TDWSCompilerTestCase;

  { TDWSCustomTest }

  TDWSCustomTest = class(TDWSCompilerTestCase)
  published
    procedure CompilationNormal;
    procedure CompilationWithMapAndSymbols;
    procedure ExecutionNonOptimized;
    procedure ExecutionOptimized;
  end;

  { TTestFolderSuite }

  TTestFolderSuite = class(TTestSuite)
  public
    constructor Create(ASuiteName, AFolder: string; AClass: TDWSCompilerTestCaseClass);
      reintroduce;
  end;

  { TTestFileSuite }

  TTestFileSuite = class(TTestSuite)
  public
    constructor Create(ATestFile: string; ATestClass: TDWSCompilerTestCaseClass);
      reintroduce;
  end;


implementation

{ TDWSTestCaseBase }

procedure TDWSTestCaseBase.SetUp;
begin
  inherited SetUp;
  FCompiler := TDelphiWebScript.Create(nil);
  FProg     := nil;
end;

procedure TDWSTestCaseBase.TearDown;
begin
  FreeAndNil(FProg);
  FreeAndNil(FCompiler);
  inherited TearDown;
end;

procedure TDWSTestCaseBase.SetOptions(const AOptions: TCompilerOptions);
begin
  FCompiler.Config.CompilerOptions := AOptions;
end;

procedure TDWSTestCaseBase.Compile(const ASource: string);
begin
  CheckNull(FProg, 'FProg before compile'); //avoid memleak in tests
  FProg := FCompiler.Compile(ASource);
end;

procedure TDWSTestCaseBase.Compile(const ASource: TStrings);
begin
  Compile(ASource.Text);
end;

procedure TDWSTestCaseBase.Execute;
begin
  FProg.Execute;
end;

procedure TDWSTestCaseBase.CheckEqualsInfo(Expected: string; msg: string);
begin
  CheckEquals(Expected, FProg.Msgs.AsInfo, msg);
end;

procedure TDWSTestCaseBase.CheckEmptyInfo(msg: string);
begin
  CheckEqualsInfo('', msg);
end;

procedure TDWSTestCaseBase.CheckEqualsResult(Expected: string; msg: string);
begin
  CheckEquals(Expected, (FProg.Result as TdwsDefaultResult).Text, msg);
end;

{ TDWSCustomTest }

procedure TDWSCustomTest.CompilationNormal;
begin
  inherited;
end;

procedure TDWSCustomTest.CompilationWithMapAndSymbols;
begin
  inherited;
end;

procedure TDWSCustomTest.ExecutionNonOptimized;
begin
  inherited;
end;

procedure TDWSCustomTest.ExecutionOptimized;
begin
  inherited;
end;


{ TTestFileSuite }

constructor TTestFileSuite.Create(ATestFile: string;
  ATestClass: TDWSCompilerTestCaseClass);
var
  ml: TStringList;
  s:  string;
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
  end;
end;

{ TTestFolderSuite }

constructor TTestFolderSuite.Create(ASuiteName, AFolder: string;
  AClass: TDWSCompilerTestCaseClass);
var
  test_data_path: string;
  FFiles: TStringList;
  s:      string;
  file_suite: TTestFileSuite;
begin
  inherited Create(ASuiteName);
  //collect tests
  test_data_path := ExtractFilePath(ParamStr(0)) + AFolder + DirectorySeparator;
  FFiles := TStringList.Create;
  try
    try
      CollectFiles(test_data_path, AClass.GetTestFileMask, FFiles);
      for s in FFiles do
      begin
        file_suite := TTestFileSuite.Create(s, AClass);
        AddTest(file_suite);
      end;
    finally
      FFiles.Free;
    end;
  except
    AddTest(Warning('Exception during test creation'));
  end;
end;

{ TDWSCompilerTestCase }

procedure TDWSCompilerTestCase.SetUp;
begin
  inherited;
  FOldDS := GetDecimalSeparator;
  SetDecimalSeparator('.');

  FSource := TStringList.Create;
  FSource.LoadFromFile(FTestFilename);

  FResultFileName := ChangeFileExt(FTestFilename, '.txt');
  FExpectedResult := TStringList.Create;
  if FileExists(FResultFileName) then
  begin
    FExpectedResult.LoadFromFile(FResultFileName);
  end
  else
  begin
    FExpectedResult.Clear;
  end;
end;

procedure TDWSCompilerTestCase.TearDown;
begin
  FExpectedResult.Free;
  FSource.Free;
  SetDecimalSeparator(FOldDS);
  inherited;
end;

procedure TDWSCompilerTestCase.Compilation;
begin
  Compile(FSource);
  CheckEmptyInfo('Info after compile');
end;

procedure TDWSCompilerTestCase.Execution;
begin
  Compilation;
  Execute;
  CheckEqualsResult(FExpectedResult.Text, 'Exec result');
  CheckEmptyInfo('Info after exec');
  PostExec;
end;

procedure TDWSCompilerTestCase.PostExec;
begin
  //do nothing in base class
end;

class function TDWSCompilerTestCase.GetTestFileMask: string;
begin
  Result := '*.pas';
end;

constructor TDWSCompilerTestCase.CreateWith(const ATestName: string;
  const ATestSuiteName: string; const ATestFile: string);
begin
  inherited CreateWith(ATestName, ATestSuiteName);
  FTestFilename := ATestFile;
end;

class function TDWSCompilerTestCase.Suite(const AName, AFolder: string): TTest;
begin
  Result := TTestFolderSuite.Create(AName, AFolder, Self);
end;

procedure TDWSCompilerTestCase.CompilationNormal;
begin
  SetOptions([coOptimize]);
  Compilation;
end;

procedure TDWSCompilerTestCase.CompilationWithMapAndSymbols;
begin
  SetOptions([coSymbolDictionary, coContextMap]);
  Compilation;
end;

procedure TDWSCompilerTestCase.ExecutionNonOptimized;
begin
  SetOptions([]);
  Execution;
end;

procedure TDWSCompilerTestCase.ExecutionOptimized;
begin
  SetOptions([coOptimize]);
  Execution;
end;

end.

