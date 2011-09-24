{$I dws_tests_conf.inc}
unit UScriptTests;

interface

uses Classes, SysUtils,
      fpcunit,testregistry,
  dws_fpcunit,
  dwsComp, dwsCompiler, dwsExprs, dwsXPlatform;

type

   { TScriptTests }

   TScriptTests = class (TDWSCustomTest)
      private
         FOldDS: char;
      public
         procedure SetUp; override;
         procedure TearDown; override;

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
   FCompiler.Config.CompilerOptions:=[coOptimize];
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
   FOldDS := GetDecimalSeparator;
   SetDecimalSeparator('.');

   FCompiler.OnInclude:=DoInclude;
end;

// TearDown
//
procedure TScriptTests.TearDown;
begin
   SetDecimalSeparator(FOldDS);
   inherited;
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
  CheckEquals(FExpectedResult.Text,output,'ouput');
end;

initialization

   RegisterTest('', TScriptTests.Suite('SimpleScripts','SimpleScripts'));
   RegisterTest('', TScriptTests.Suite('Algorithms','Algorithms'));
   RegisterTest('', TScriptFailureTests.Suite('FailureScripts','FailureScripts'));

end.
