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
var
   expectedError : TStringList;
   expectedErrorsFileName : String;
begin
      //TODO: refactor code duplication with base class
   FCompiler.Config.CompilerOptions:=[coOptimize];
   expectedError:=TStringList.Create;
   try

         FProg:=FCompiler.Compile(FSource.Text);
            expectedErrorsFileName:=ChangeFileExt(FTestFilename, '.txt');
            if FileExists(expectedErrorsFileName) then begin
               expectedError.LoadFromFile(expectedErrorsFileName);
               CheckEquals(expectedError.Text, FProg.Msgs.AsInfo, FTestFilename);
            end else Check(fprog.Msgs.AsInfo<>'', FTestFilename+': undetected error');


   finally
      expectedError.Free;
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
    expectedResult : TStringList;
   resultsFileName : String;
   output : String;
begin
   //TODO: refactor code duplication with base class
   expectedResult:=TStringList.Create;
   try
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
            resultsFileName:=ChangeFileExt(FTestFilename, '.txt');
            if FileExists(resultsFileName) then begin
               expectedResult.LoadFromFile(resultsFileName);
               CheckEquals(expectedResult.Text, output, FTestFilename);
            end else CheckEquals('', output, FTestFilename);


   finally
      expectedResult.Free;
   end;
end;

initialization

   RegisterTest('', TScriptTests.Suite('SimpleScripts','SimpleScripts'));
   RegisterTest('', TScriptTests.Suite('Algorithms','Algorithms'));
   RegisterTest('', TScriptFailureTests.Suite('FailureScripts','FailureScripts'));

end.
