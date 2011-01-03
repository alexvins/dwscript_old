unit UHTMLFilterTests;

interface

uses
  Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs,
  dwsHtmlFilter, dwsXPlatform;

type

   THTMLFilterTests = class(TTestCase)
   private
      FTests: TStringList;
      FCompiler: TDelphiWebScript;
      FFilter: TdwsHTMLFilter;
      FUnit: TdwsHTMLUnit;

   public
      procedure SetUp; override;
      procedure TearDown; override;

      procedure DoInclude(const scriptName: string; var scriptSource: string);

   published
      procedure TestHTMLScript;

   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ THTMLFilterTests ------------------
// ------------------

// SetUp
//
procedure THTMLFilterTests.SetUp;
begin
  FTests := TStringList.Create;

  CollectFiles(ExtractFilePath(ParamStr(0)) + 'HTMLFilterScripts' + PathDelim, '*.dws', FTests);

  FCompiler := TDelphiWebScript.Create(nil);
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
//
procedure THTMLFilterTests.TearDown;
begin
  FCompiler.Free;
  FFilter.Free;
  FUnit.Free;
  FTests.Free;
end;

procedure THTMLFilterTests.TestHTMLScript;
var
  s: string;
  resultFileName : String;
  prog: TdwsProgram;
  sl : TStringList;
begin
   sl:=TStringList.Create;
   try
      for s in FTests do begin
         sl.LoadFromFile(s);
         prog := FCompiler.Compile(sl.Text);
         try
            CheckEquals('', prog.Msgs.AsInfo, s);
            prog.Execute;

            resultFileName:=ChangeFileExt(s, '.txt');
            if FileExists(resultFileName) then
               sl.LoadFromFile(ChangeFileExt(resultFileName, '.txt'))
            else sl.Clear;
            CheckEquals(sl.Text, (prog.Result as TdwsDefaultResult).Text, s);
         finally
            prog.Free;
         end;
      end;
   finally
      sl.Free;
   end;
end;

// DoInclude
//
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

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

TestFrameWork.RegisterTest('HTMLFilterTests', THTMLFilterTests.Suite);

end.
