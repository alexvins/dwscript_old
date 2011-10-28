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
    FProg:     IdwsProgram;
    FExec:     IdwsProgramExecution;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    procedure AddScriptPath(const APath: UnicodeString);
    procedure SetOptions(const AOptions: TCompilerOptions);
    procedure SetMaxRecursionDepth(AValue: Integer);
    procedure SetMaxDataSize(AValue: Integer);
    procedure Compile(const ASource: UnicodeString);
    procedure Compile(const ASource: TStrings);
    procedure Execute;
    procedure ExecuteWTimeout(TimeOut: Integer);

    procedure CheckEqualsInfo(Expected: UnicodeString; msg: UnicodeString = '');
    procedure CheckEmptyInfo(msg: UnicodeString = '');
    //default result as text
    procedure CheckEqualsResult(Expected: UnicodeString; msg: UnicodeString = '');
    procedure CheckCompile(const ASource: UnicodeString);
    procedure CheckCompile(const ASource: TStrings);

  end;

  { TDWSCompilerTestCase }

  TDWSCompilerTestCase = class(TDWSTestCaseBase)
  private
    FOldDS: Char;
  strict protected
    FTestFilename: UnicodeString;
    FSource: TStringList;
    FResultFileName: UnicodeString;
    FExpectedResult: TStringList;
  protected
    property TestFilename: UnicodeString read FTestFilename;
    procedure SetUp; override;
    procedure TearDown; override;

    procedure Compilation; virtual;
    procedure Execution; virtual;

    procedure PostExec; virtual;

    class function GetTestFileMask: UnicodeString; virtual;

  public
    constructor CreateWith(const ATestName: UnicodeString; const ATestSuiteName: UnicodeString;
      const ATestFile: UnicodeString); reintroduce; virtual;
    class function Suite(const AName, AFolder: UnicodeString): TTest;
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
    constructor Create(ASuiteName, AFolder: UnicodeString; AClass: TDWSCompilerTestCaseClass);
      reintroduce;
  end;

  { TTestFileSuite }

  TTestFileSuite = class(TTestSuite)
  public
    constructor Create(ATestFile: UnicodeString; ATestClass: TDWSCompilerTestCaseClass);
      reintroduce;
  end;


implementation

{ TDWSTestCaseBase }

procedure TDWSTestCaseBase.SetUp;
begin
  inherited SetUp;
  FCompiler := TDelphiWebScript.Create(nil);
  FProg     := nil;
  FExec := nil;
end;

procedure TDWSTestCaseBase.TearDown;
begin
  FExec := nil;
  FProg := nil;
  FreeAndNil(FCompiler);
  inherited TearDown;
end;

procedure TDWSTestCaseBase.AddScriptPath(const APath: UnicodeString);
begin
  FCompiler.Config.ScriptPaths.Add(APath);
end;

procedure TDWSTestCaseBase.SetOptions(const AOptions: TCompilerOptions);
begin
  FCompiler.Config.CompilerOptions := AOptions;
end;

procedure TDWSTestCaseBase.SetMaxRecursionDepth(AValue: Integer);
begin
  FCompiler.Config.MaxRecursionDepth := AValue;
end;

procedure TDWSTestCaseBase.SetMaxDataSize(AValue: Integer);
begin
  FCompiler.Config.MaxDataSize := AValue;
end;

procedure TDWSTestCaseBase.Compile(const ASource: UnicodeString);
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
  Fexec := FProg.Execute;
end;

procedure TDWSTestCaseBase.ExecuteWTimeout(TimeOut: Integer);
begin
  FProg.TimeoutMilliseconds := TimeOut;
  Execute;
end;

procedure TDWSTestCaseBase.CheckEqualsInfo(Expected: UnicodeString; msg: UnicodeString);
begin
  CheckEquals(Expected, FProg.Msgs.AsInfo, 'Prog.Info ' + msg);
end;

procedure TDWSTestCaseBase.CheckEmptyInfo(msg: UnicodeString);
begin
  CheckEqualsInfo('', msg);
end;

procedure TDWSTestCaseBase.CheckEqualsResult(Expected: UnicodeString; msg: UnicodeString);
begin
  CheckEquals(Expected, (FExec.Result as TdwsDefaultResult).Text,
    'Prog default result ' + msg);
end;

procedure TDWSTestCaseBase.CheckCompile(const ASource: UnicodeString);
begin
  Compile(ASource);
  CheckEmptyInfo('Compilation error');
end;

procedure TDWSTestCaseBase.CheckCompile(const ASource: TStrings);
begin
  CheckCompile(ASource.Text);
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

constructor TTestFileSuite.Create(ATestFile: UnicodeString;
  ATestClass: TDWSCompilerTestCaseClass);
var
  ml: TStringList;
  s:  UnicodeString;
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

constructor TTestFolderSuite.Create(ASuiteName, AFolder: UnicodeString;
  AClass: TDWSCompilerTestCaseClass);
var
  test_data_path: UnicodeString;
  FFiles: TStringList;
  s:      UnicodeString;
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
  end else
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
  CheckCompile(FSource);
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

class function TDWSCompilerTestCase.GetTestFileMask: UnicodeString;
begin
  Result := '*.pas';
end;

constructor TDWSCompilerTestCase.CreateWith(const ATestName: UnicodeString;
  const ATestSuiteName: UnicodeString; const ATestFile: UnicodeString);
begin
  inherited CreateWith(ATestName, ATestSuiteName);
  FTestFilename := ATestFile;
end;

class function TDWSCompilerTestCase.Suite(const AName, AFolder: UnicodeString): TTest;
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

