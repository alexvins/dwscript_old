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

    procedure Execution; virtual; abstract;
    procedure Compilation; virtual; abstract;


    //class function GetTestDataPath: string; virtual; abstract;
    class function GetTestFileMask: string; virtual;
  public
    constructor CreateWith(const ATestName: string;
      const ATestSuiteName: string; const ATestFile: String); reintroduce;virtual;
    class function Suite (const AName, AFolder:string): TTest;
  published
    procedure CompilationNormal;
    procedure CompilationWithMapAndSymbols;
    procedure ExecutionNonOptimized;
    procedure ExecutionOptimized;
  end;

  TDWSCompilerTestCaseClass = class of TDWSCompilerTestCase;

  { TTestFolderSuite }

  TTestFolderSuite = class (TTestSuite)
  public
    constructor Create(ASuiteName, AFolder: string; AClass: TDWSCompilerTestCaseClass); reintroduce;
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

{ TTestFolderSuite }

constructor TTestFolderSuite.Create(ASuiteName, AFolder: string;
  AClass: TDWSCompilerTestCaseClass);
var
  test_data_path: string;
  FFiles: TStringList;
  s: String;
  file_suite: TTestFileSuite;
begin
  inherited Create (ASuiteName);
  //collect tests
  test_data_path := ExtractFilePath(ParamStr(0))
    + AFolder+DirectorySeparator;
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

{ TDWSCompilerTestCase }

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

class function TDWSCompilerTestCase.Suite(const AName, AFolder: string): TTest;
begin
  Result := TTestFolderSuite.Create(AName, AFolder, Self);
end;

procedure TDWSCompilerTestCase.CompilationNormal;
begin
     FCompiler.Config.CompilerOptions:=[coOptimize];
   Compilation;

end;

procedure TDWSCompilerTestCase.CompilationWithMapAndSymbols;
begin
    FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap];
   Compilation;
end;

procedure TDWSCompilerTestCase.ExecutionNonOptimized;
begin
  FCompiler.Config.CompilerOptions:=[];
  Execution;
end;

procedure TDWSCompilerTestCase.ExecutionOptimized;
begin
  FCompiler.Config.CompilerOptions:=[coOptimize];
  Execution;
end;

end.
