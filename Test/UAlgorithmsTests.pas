unit UAlgorithmsTests;

interface

uses Classes, SysUtils, dwsXPlatformTests, dwsCompiler, dwsExprs,
   dwsXPlatform, dwsUtils, dwsSymbols,dws_fpcunit;

type

   TAlgorithmsTests = class (TDWSCustomTest)
      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure Execution; override;
         procedure Compilation; override;

   end;

   { TAlgorithmsThreadedTests }

   TAlgorithmsThreadedTests = class (TDWSTestCaseBase)
      private
         FTests : TStringList;
      public
         procedure SetUp; override;
         procedure TearDown; override;
      published

         procedure ExecutionThreaded;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

procedure EmptyCallBack(parent, expr : TExprBase; var abort : Boolean);
begin
   // just used for detecting crashes in subexpr tree navigation
end;

type

   TThreadedRunner = class(TdwsThread)
      Script : UnicodeString;
      Exec : IdwsProgramExecution;
      Count : Integer;
      ExpectedResult : UnicodeString;
      ActualResult : UnicodeString;

      procedure Execute; override;
   end;

// Execute
//
procedure TThreadedRunner.Execute;
begin
   while Count>0 do begin
      try
         Exec.Execute;
      except
         on E: Exception do begin
            ActualResult:=E.ClassName+': '+E.Message;
            Break;
         end;
      end;
      ActualResult:=Exec.Result.ToString;
      if Copy(ActualResult, 1, 5)='Swaps' then
         ActualResult:=Copy(ActualResult, Pos(#13#10, ActualResult)+2, MaxInt);
      if ActualResult<>ExpectedResult then
         Break;
      Dec(Count);
   end;
end;

// ------------------
// ------------------ TAlgorithmsTests ------------------
// ------------------

// SetUp
//
procedure TAlgorithmsTests.SetUp;
begin
   inherited;
end;

// TearDown
//
procedure TAlgorithmsTests.TearDown;
begin
   inherited;
end;

// Compilation
//
procedure TAlgorithmsTests.Compilation;
begin
   inherited;
   fprog.GetProgramObject.InitExpr.RecursiveEnumerateSubExprs(EmptyCallBack);
   fprog.GetProgramObject.Expr.RecursiveEnumerateSubExprs(EmptyCallBack);
end;



// Execution
//
procedure TAlgorithmsTests.Execution;
begin
   Compilation;
   Execute;
   CheckEmptyInfo('Info after exec');
   CheckEqualsOutput(FExpectedResult,'Exec result');
   PostExec;
end;


{ TAlgorithmsThreadedTests }

procedure TAlgorithmsThreadedTests.ExecutionThreaded;
const
   cRunsPerThread = 15;
   cThreadsPerScript = 3;
   cMaxScriptsAtATime = 8;
var
   source, expectedResult : TStringList;
   i, j, k : Integer;
   prog : IdwsProgram;
   threads : array of TThreadedRunner;
   runner : TThreadedRunner;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize, coAssertions];

   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      SetLength(threads, cMaxScriptsAtATime*cThreadsPerScript);

      i:=0;
      while i<FTests.Count do begin

         for k:=0 to cMaxScriptsAtATime-1 do begin

            if i>=FTests.Count then begin
               for j:=0 to cThreadsPerScript-1 do
                  threads[k*cThreadsPerScript+j]:=nil;
               continue;
            end;

            source.LoadFromFile(FTests[i]);
            prog:=FCompiler.Compile(source.Text);

            if prog.Msgs.HasErrors then continue;

            expectedResult.LoadFromFile(ChangeFileExt(FTests[i], '.txt'));
            if Copy(expectedResult[0], 1, 5)='Swaps' then
               expectedResult.Delete(0); // variable part because of randomization

            // prepare threads
            for j:=0 to cThreadsPerScript-1 do begin
               runner:=TThreadedRunner.Create(True);
               runner.FreeOnTerminate:=False;
               runner.Count:=cRunsPerThread;
               runner.Script:=Format('%s [%d]', [ExtractFileName(FTests[i]), j]);
               runner.Exec:=prog.CreateNewExecution;
               runner.ExpectedResult:=expectedResult.Text;
               threads[k*cThreadsPerScript+j]:=runner;
            end;

            // unleash threads
            for j:=0 to cThreadsPerScript-1 do
               threads[k*cThreadsPerScript+j].Start;

            Inc(i);

         end;

         // wait for completion and check for failures
         try
            for k:=0 to High(threads) do begin
               runner:=threads[k];
               if runner<>nil then begin
                  runner.WaitFor;
                  CheckEquals(runner.ExpectedResult, runner.ActualResult, 'Thread failure for '+runner.Script);
               end;
            end;
         finally
            for k:=0 to High(threads) do
               FreeAndNil(threads[k]);
         end;

      end;

   finally
      source.Free;
      expectedResult.Free;
   end;
end;

procedure TAlgorithmsThreadedTests.SetUp;
begin
   inherited SetUp;
   OverrideDecimalSeparator;

   FTests:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'Algorithms'+PathDelim, '*.pas', FTests);

end;

procedure TAlgorithmsThreadedTests.TearDown;
begin
   RestoreDecimalSeparator;
   FTests.Free;
   inherited TearDown;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
   RegisterTest('Algorithms', TAlgorithmsTests.Suite('Files', 'Algorithms'));
   //RegisterTest('AlgorithmsTests', TAlgorithmsTests);
   RegisterTest('Algorithms', TAlgorithmsThreadedTests);

end.
