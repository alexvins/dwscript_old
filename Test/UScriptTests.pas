{$I dws_tests_conf.inc}
unit UScriptTests;

interface

uses Classes,testregistry,
  dws_fpcunit, dwsCompiler;

type

  { TScriptTests }

  TScriptTests = class (TDWSCustomTest)
  public
    procedure SetUp; override;
    procedure DoInclude(const scriptName: string; var scriptSource: string);
    procedure Execution; override;
  end;


   { TScriptFailureTests }

   TScriptFailureTests = class (TDWSCompilerTestCase)
   published
     procedure CompilationFailure;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation

{ TScriptFailureTests }

procedure TScriptFailureTests.CompilationFailure;
begin
  SetOptions([coOptimize]);
   FProg:=FCompiler.Compile(FSource.Text);

   if FExpectedResult.Count=0 then
   begin
      Check(fprog.Msgs.AsInfo<>'', 'undetected error');
   end else
   begin
     CheckEquals(FExpectedResult.Text, FProg.Msgs.AsInfo, 'Error messages');
   end;
end;

{ TScriptTests }

procedure TScriptTests.SetUp;
begin
   inherited;

   FCompiler.OnInclude:=DoInclude;
end;

// DoInclude
//
procedure TScriptTests.DoInclude(const scriptName: string; var scriptSource: string);
var
   sl : TStringList;
begin
   sl:=TStringList.Create;
   try
      sl.LoadFromFile('SimpleScripts\'+scriptName);
      scriptSource:=sl.Text;
   finally
      sl.Free;
   end;
end;


procedure TScriptTests.Execution;
var
   output : String;
begin
  Compilation;
  fprog.Execute;
  if fprog.Msgs.Count=0 then
     output:=(fprog.Result as TdwsDefaultResult).Text
  else begin
     output:= 'Errors >>>>'#13#10
             +fprog.Msgs.AsInfo
             +'Result >>>>'#13#10
             +(fprog.Result as TdwsDefaultResult).Text;
  end;
  CheckEquals(FExpectedResult.Text,output,'output');
end;

initialization

   RegisterTest('', TScriptTests.Suite('SimpleScripts','SimpleScripts'));
   //RegisterTest('', TScriptTests.Suite('Algorithms','Algorithms'));
   RegisterTest('', TScriptFailureTests.Suite('FailureScripts','FailureScripts'));

end.
