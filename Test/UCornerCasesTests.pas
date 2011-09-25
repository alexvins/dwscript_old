{$I dws_tests_conf.inc}
unit UCornerCasesTests;

interface

uses Classes, SysUtils,
  fpcunit, testregistry,
  dws_fpcunit,
  dwsComp, dwsCompiler, dwsExprs,
  dwsTokenizer, dwsXPlatform, dwsFileSystem, dwsErrors;

type

  { TCornerCasesTests }

  TCornerCasesTests = class(TDWSTestCaseBase)
  published
    procedure EmptyTokenBuffer;
    procedure IgnoreDecimalSeparator;
    procedure TokenizerSpecials;
    procedure TimeOutTestFinite;
    procedure TimeOutTestInfinite;
    procedure StackMaxRecursion;
    procedure StackOverFlow;
  end;

  { TIncludeTestCase }

  TIncludeTestCase = class abstract(TDWSTestCaseBase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    procedure SetOnInclude;
  public
    procedure DoOnInclude(const scriptName: string; var scriptSource: string);
  end;

  { TIncludeViaEventTests }

  TIncludeViaEventTests = class(TIncludeTestCase)
  published
    procedure MissingIncludeName;
    procedure IncludeForbidden;
    procedure IncludeOK;
  end;

  { TIncludeViaFileTests }

  TRestrictedTestMethod = procedure of object;

  TIncludeViaFileTests = class(TIncludeTestCase)
  private
    FDummiFileName: string;
    FTempDir: string;
    FRestricted: TdwsRestrictedFileSystem;

    procedure BeginRestricted;
    procedure EndRestricted;
    procedure DoRestricted(const AMethod: TRestrictedTestMethod);

    procedure DoIncludeRestrictedNoPaths;
    procedure DoIncludeRestrictedPath;
    procedure DoIncludeRestrictedOK;

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure IncludeNoPaths;
    procedure IncludePathOK;

    procedure IncludeRestrictedNoPaths;
    procedure IncludeRestrictedPath;
    procedure IncludeRestrictedOK;
  end;

 // ------------------------------------------------------------------
 // ------------------------------------------------------------------
 // ------------------------------------------------------------------
implementation
 // ------------------------------------------------------------------
 // ------------------------------------------------------------------
 // ------------------------------------------------------------------


type
  // TTokenBufferWrapper

  TTokenBufferWrapper = class
    Buffer: TTokenBuffer;
  end;

{ TIncludeViaFileTests }

procedure TIncludeViaFileTests.BeginRestricted;
begin
  CheckNull(FRestricted,'BeginRestricted');
  FRestricted := TdwsRestrictedFileSystem.Create(nil);
  FCompiler.Config.CompileFileSystem := FRestricted;
end;

procedure TIncludeViaFileTests.EndRestricted;
begin
  FreeAndNil(FRestricted);
  CheckNull(FCompiler.Config.CompileFileSystem, 'Notification release');
end;

procedure TIncludeViaFileTests.DoRestricted(const AMethod: TRestrictedTestMethod
  );
begin
  try
    BeginRestricted;
    AMethod();
  finally
    EndRestricted;
  end;
end;

procedure TIncludeViaFileTests.DoIncludeRestrictedNoPaths;
begin
  FRestricted.Paths.Text := FTempDir +DirectorySeparator+ 'nothing';
  Compile('{$include ''test.dummy''}');
  CheckEqualsInfo('Compile Error: Couldn''t find file "test.dummy" on input paths [line: 1, column: 11]'#13#10,
    'after compile');
end;

procedure TIncludeViaFileTests.DoIncludeRestrictedPath;
begin
  FRestricted.Paths.Text := FTempDir;
  Compile('{$include ''test.dummy''}');

  CheckEqualsInfo('Compile Error: Couldn''t find file "test.dummy" on input paths [line: 1, column: 11]'#13#10,
     'include via file restricted - no paths');
end;

procedure TIncludeViaFileTests.DoIncludeRestrictedOK;
begin
  FRestricted.Paths.Text := FTempDir;
  AddScriptPath('.');
  CheckCompile('{$include ''test.dummy''}');
  Execute;
  CheckEqualsResult('world');
end;

procedure TIncludeViaFileTests.SetUp;
var
  fs: TFileStream;
  s: string;
begin
  inherited SetUp;
  FTempDir  := TPath.GetTemporaryFilesPath;
  FDummiFileName := FTempDir + 'test.dummy';

  if FileExists(FDummiFileName) then DeleteFile(FDummiFileName);

  fs := TFileStream.Create(FDummiFileName,fmCreate+fmOpenReadWrite);
  try
    s :='Print(''world'');';
    fs.Write(s[1],Length(s));
  finally
    fs.Free;
  end;
end;

procedure TIncludeViaFileTests.TearDown;
begin
  if FileExists(FDummiFileName) then DeleteFile(FDummiFileName);
  inherited TearDown;
end;

procedure TIncludeViaFileTests.IncludeNoPaths;
begin
  Compile('{$include ''test.dummy''}');
  CheckEqualsInfo('Compile Error: Couldn''t find file "test.dummy" on input paths [line: 1, column: 11]'#13#10,
      'include via file no paths');
end;

procedure TIncludeViaFileTests.IncludePathOK;
begin
  AddScriptPath(FTempDir);
  Compile('{$include ''test.dummy''}');
  CheckEmptyInfo('After compile');
  Execute;
  CheckEqualsResult('world','Exec result');
end;

procedure TIncludeViaFileTests.IncludeRestrictedNoPaths;
begin
  DoRestricted(DoIncludeRestrictedNoPaths);
end;

procedure TIncludeViaFileTests.IncludeRestrictedPath;
begin
  DoRestricted(DoIncludeRestrictedPath);
end;

procedure TIncludeViaFileTests.IncludeRestrictedOK;
begin
  DoRestricted(DoIncludeRestrictedOK);
end;

{ TIncludeViaEventTests }

procedure TIncludeViaEventTests.MissingIncludeName;
begin
  Compile('{$include}');
  CheckEqualsInfo('Syntax Error: Name of include file expected [line: 1, column: 10]'#13#10,
    'include missing');
end;

procedure TIncludeViaEventTests.IncludeForbidden;
begin
  Compile('{$include ''test.dummy''}');
  CheckEqualsInfo('Compile Error: Couldn''t find file "test.dummy" on input paths [line: 1, column: 11]'#13#10,
    'include forbidden');
end;

procedure TIncludeViaEventTests.IncludeOK;
begin
  SetOnInclude;
  Compile('{$include ''test.dummy''}');
  CheckEmptyInfo('include via event');
  Execute;
  CheckEqualsResult('hello', 'exec include via event');
end;

{ TIncludeTestCase }

procedure TIncludeTestCase.SetUp;
begin
  inherited SetUp;
  FCompiler.OnInclude := nil;
  FCompiler.Config.ScriptPaths.Clear;
end;

procedure TIncludeTestCase.TearDown;
begin
  inherited TearDown;
end;

procedure TIncludeTestCase.SetOnInclude;
begin
  FCompiler.OnInclude := DoOnInclude;
end;

procedure TIncludeTestCase.DoOnInclude(const scriptName: string;
  var scriptSource: string);
begin
  CheckEquals('test.dummy', scriptName, 'DoOnInclude');
  scriptSource := 'Print(''hello'');';
end;


 // ------------------
 // ------------------ TCornerCasesTests ------------------
 // ------------------

// EmptyTokenBuffer

procedure TCornerCasesTests.EmptyTokenBuffer;
var
  w: TTokenBufferWrapper;
  s: string;
begin
  w := TTokenBufferWrapper.Create;
  try
    CheckEquals('', w.Buffer.ToStr, 'ToStr function');
    s := 'dummy';
    w.Buffer.ToStr(s);
    CheckEquals('', s, 'ToStr procedure');
    s := 'dummy';
    w.Buffer.ToUpperStr(s);
    CheckEquals('', s, 'ToUpperStr');
    CheckEquals(#0, w.Buffer.LastChar, 'LastChar');
  finally
    w.Free;
  end;
end;

// IgnoreDecimalSeparator

procedure TCornerCasesTests.IgnoreDecimalSeparator;
var
  w:  TTokenBufferWrapper;
  dc: Char;
begin
  w  := TTokenBufferWrapper.Create;
  dc := GetDecimalSeparator;
  try
    w.Buffer.AppendChar('1');
    w.Buffer.AppendChar('.');
    w.Buffer.AppendChar('5');

    SetDecimalSeparator('.');
    CheckEquals(1.5, w.Buffer.ToFloat, 'With dot');
    SetDecimalSeparator(',');
    CheckEquals(1.5, w.Buffer.ToFloat, 'With comma');
    SetDecimalSeparator('P');
    CheckEquals(1.5, w.Buffer.ToFloat, 'With P');

  finally
    SetDecimalSeparator(dc);
    w.Free;
  end;
end;

// TokenizerSpecials

procedure TCornerCasesTests.TokenizerSpecials;
var
  t:    TTokenizer;
  msgs: TdwsMessageList;
begin
  msgs := TdwsMessageList.Create;
  t    := TTokenizer.Create('@ @= %= ^ ^=', '', msgs);
  try
    CheckTrue(t.TestDelete(ttAT), '@');
    CheckTrue(t.TestDelete(ttAT_ASSIGN), '@=');
    CheckTrue(t.TestDelete(ttPERCENT_ASSIGN), '%=');
    CheckTrue(t.TestDelete(ttCARET), '^');
    CheckTrue(t.TestDelete(ttCARET_ASSIGN), '^=');

    CheckTrue(t.TestAny([ttNAME]) = ttNone, 'Any at end');
    CheckTrue(t.TestDeleteAny([ttNAME]) = ttNone, 'DeleteAny at end');

  finally
    t.Free;
    msgs.Free;
  end;
end;

// TimeOutTestFinite

procedure TCornerCasesTests.TimeOutTestFinite;
begin
  Compile('while false do;');
  ExecuteWTimeout(1000);
end;

// TimeOutTestInfinite

procedure TCornerCasesTests.TimeOutTestInfinite;
begin
  Compile('while true do;');
  ExecuteWTimeout(100);
end;

// StackMaxRecursion

procedure TCornerCasesTests.StackMaxRecursion;
const
  script = 'procedure Dummy; begin Dummy; end; Dummy;';
begin
  SetMaxRecursionDepth(20);
  CheckCompile(script);
  Execute;
  CheckEqualsInfo('Runtime Error: Maximal recursion exceeded (20 calls)'#13#10,
       'after execute');
end;

// StackOverFlow

procedure TCornerCasesTests.StackOverFlow;
const
  script = 'procedure Dummy; var i : Integer; begin Dummy; end; Dummy;';
begin
  SetMaxDataSize(1024);
  CheckCompile(script);
  Execute;
  CheckEqualsInfo('Runtime Error: Maximal data size exceeded (64 Variants)'#13#10,
       'after execute');
end;

 // ------------------------------------------------------------------
 // ------------------------------------------------------------------
 // ------------------------------------------------------------------
initialization
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------

  RegisterTest('CornerCasesTests', TCornerCasesTests.Suite);
  RegisterTest('CornerCasesTests', TIncludeViaEventTests.Suite);
  RegisterTest('CornerCasesTests', TIncludeViaFileTests.Suite);

end.


