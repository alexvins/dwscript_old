unit UdwsFunctionsTests;

interface

uses Classes, SysUtils,
  fpcunit,testregistry,
  dws_fpcunit,
  dwsComp, dwsCompiler, dwsExprs,
  dwsTokenizer, dwsSymbols, dwsMathFunctions, dwsTimeFunctions,
  dwsVariantFunctions;

type

  { TdwsFunctionsTests }

  TdwsFunctionsTests = class (TDWSCompilerTestCase)
  public
     procedure Compilation; override;
     procedure Execution; override;
  end;


implementation

// ------------------
// ------------------ TdwsFunctionsTests ------------------
// ------------------

procedure TdwsFunctionsTests.Compilation;
var
   source : TStringList;
   //i : Integer;
   prog : TdwsProgram;
begin
   source:=TStringList.Create;
   try

      //for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTestFilename);

         prog:=FCompiler.Compile(source.Text);
         try
            CheckEquals('', prog.Msgs.AsInfo, FTestFilename);
         finally
            prog.Free;
         end;

        //end;

   finally
      source.Free;
   end;
end;

// Execution
//
procedure TdwsFunctionsTests.Execution;
var
   source, expectedResult : TStringList;
   //i : Integer;
   prog : TdwsProgram;
   resultsFileName : String;
begin
  source:=TStringList.Create;
  expectedResult:=TStringList.Create;
  try
    source.LoadFromFile(FTestFilename);

    prog:=FCompiler.Compile(source.Text);
    try
      CheckEquals('', prog.Msgs.AsInfo, FTestFilename);
      prog.Execute;
      CheckEquals('', prog.Msgs.AsInfo, FTestFilename);
      resultsFileName:=ChangeFileExt(FTestFilename, '.txt');
      if FileExists(resultsFileName) then begin
         expectedResult.LoadFromFile(resultsFileName);
         CheckEquals(expectedResult.Text, (prog.Result as TdwsDefaultResult).Text, FTestFilename);
      end else CheckEquals('', (prog.Result as TdwsDefaultResult).Text, FTestFilename);
      CheckEquals('', prog.Msgs.AsInfo, FTestFilename);
    finally
      prog.Free;
    end;
  finally
    expectedResult.Free;
    source.Free;
  end;
end;

initialization

   RegisterTest('Functions', TdwsFunctionsTests.Suite('Math','FunctionsMath'));
   RegisterTest('Functions', TdwsFunctionsTests.Suite('Time','FunctionsTime'));
   RegisterTest('Functions', TdwsFunctionsTests.Suite('String','FunctionsString'));
   RegisterTest('Functions', TdwsFunctionsTests.Suite('Variant','FunctionsVariant'));

end.
