unit UAlgorithmsTests;

interface

uses Classes, SysUtils,
  fpcunit,testregistry,
  dws_fpcunit,
  dwsComp, dwsCompiler, dwsExprs, dwsXPlatform;

type

  { TAlgorithmsTests }

  TAlgorithmsTests = class (TDWSCompilerTestCase)
  protected
    //class function GetTestDataPath: string; override;
  public
     procedure Execution; override;
     procedure Compilation; override;

  end;

implementation

{ TAlgorithmsTests }

//class function TAlgorithmsTests.GetTestDataPath: string;
//begin
//  Result := 'Algorithms';
//end;

procedure TAlgorithmsTests.Compilation;
var
  source : TStringList;
  prog : TdwsProgram;
begin
  source:=TStringList.Create;
  try
    source.LoadFromFile(FTestFilename);

    prog:=FCompiler.Compile(source.Text);
    try
      CheckEquals('', prog.Msgs.AsInfo, FTestFilename);
    finally
      prog.Free;
    end;

  finally
    source.Free;
  end;
end;

procedure TAlgorithmsTests.Execution;
var
   source, expectedResult : TStringList;
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

   RegisterTest('',TAlgorithmsTests.Suite('AlgorithmsTests','Algorithms'));

end.
