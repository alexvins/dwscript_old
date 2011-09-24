unit UAlgorithmsTests;

interface

uses Classes, SysUtils,
  fpcunit,testregistry,
  dws_fpcunit,
  dwsComp, dwsCompiler, dwsExprs, dwsXPlatform;

type

   { TAlgorithmsTests }

   TAlgorithmsTests = class (TDWSCompilerTestCase)
      private
         //FTests : TStringList;
      protected
        class function GetTestDataPath: string; override;
      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure Execution;
         procedure Compilation;

      published

         procedure CompilationNormal;
         procedure CompilationWithMapAndSymbols;
         procedure ExecutionNonOptimized;
         procedure ExecutionOptimized;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TAlgorithmsTests ------------------
// ------------------

class function TAlgorithmsTests.GetTestDataPath: string;
begin
  Result := 'Algorithms';
end;

// SetUp
//
procedure TAlgorithmsTests.SetUp;
begin
   inherited;
   //FTests:=TStringList.Create;

   //CollectFiles(ExtractFilePath(ParamStr(0))+'Algorithms'+PathDelim, '*.pas', FTests);

end;

// TearDown
//
procedure TAlgorithmsTests.TearDown;
begin
   //FTests.Free;
   inherited;
end;

// Compilation
//
procedure TAlgorithmsTests.Compilation;
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

// CompilationNormal
//
procedure TAlgorithmsTests.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TAlgorithmsTests.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TAlgorithmsTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=[];
   Execution;
end;

// ExecutionOptimized
//
procedure TAlgorithmsTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Execution;
end;

// Execution
//
procedure TAlgorithmsTests.Execution;
var
   source, expectedResult : TStringList;
   //i : Integer;
   prog : TdwsProgram;
   resultsFileName : String;
begin
   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      //for i:=0 to FTests.Count-1 do begin

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

      //end;

   finally
      expectedResult.Free;
      source.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('AlgorithmsTests', TAlgorithmsTests.Suite);

end.
