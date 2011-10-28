unit UDebuggerTests;

interface

uses Windows, Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs,
   dwsComConnector, Variants, ActiveX, ComObj, dwsXPlatform, dwsUtils,
   dwsSymbols, dwsDebugger, dwsStrings;

type

   TDebuggerTests = class (TTestCase)
      private
         FCompiler : TDelphiWebScript;
         FUnits : TdwsUnit;
         FDebugger : TdwsDebugger;

         FDebugEvalAtLine : Integer;
         FDebugEvalExpr : UnicodeString;
         FDebugLastEvalResult : UnicodeString;

         procedure DoCreateExternal(Info: TProgramInfo; var ExtObject: TObject);
         procedure DoCleanupExternal(externalObject : TObject);
         procedure DoGetValue(Info: TProgramInfo; ExtObject: TObject);

         procedure DoDebugEval(exec: TdwsExecution; expr: TExprBase);

      public
         procedure SetUp; override;
         procedure TearDown; override;

      published
         procedure EvaluateSimpleTest;
         procedure EvaluateOutsideOfExec;
         procedure EvaluateContextTest;

         procedure ExecutableLines;

         procedure AttachToScript;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   TTestObject = class
      FField : UnicodeString;
      constructor Create(const value : UnicodeString);
   end;

// Create
//
constructor TTestObject.Create(const value : UnicodeString);
begin
   FField:=value;
end;

// ------------------
// ------------------ TDebuggerTests ------------------
// ------------------

// SetUp
//
procedure TDebuggerTests.SetUp;
var
   cls : TdwsClass;
   cst : TdwsConstructor;
   meth : TdwsMethod;
begin
   FCompiler:=TDelphiWebScript.Create(nil);
   FUnits:=TdwsUnit.Create(nil);
   FUnits.UnitName:='TestUnit';
   FUnits.Script:=FCompiler;
   FDebugger:=TdwsDebugger.Create(nil);
   FDebugger.OnDebug:=DoDebugEval;

   cls:=FUnits.Classes.Add;
   cls.Name:='TTestClass';
   cls.OnCleanUp:=DoCleanupExternal;
   cst:=cls.Constructors.Add as TdwsConstructor;
   cst.Name:='Create';
   cst.OnEval:=DoCreateExternal;

   meth:=cls.Methods.Add as TdwsMethod;
   meth.ResultType:='UnicodeString';
   meth.Name:='GetValue';
   meth.OnEval:=DoGetValue;
end;

// TearDown
//
procedure TDebuggerTests.TearDown;
begin
   FDebugger.Free;
   FUnits.Free;
   FCompiler.Free;
end;

// DoCreateExternal
//
procedure TDebuggerTests.DoCreateExternal(Info: TProgramInfo; var ExtObject: TObject);
begin
   ExtObject:=TTestObject.Create('Hello');
end;

// DoCleanupExternal
//
procedure TDebuggerTests.DoCleanupExternal(externalObject : TObject);
begin
   externalObject.Free;
end;

// DoGetValue
//
procedure TDebuggerTests.DoGetValue(Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=(ExtObject as TTestObject).FField;
end;

// DoDebugEval
//
procedure TDebuggerTests.DoDebugEval(exec: TdwsExecution; expr: TExprBase);
begin
   if expr.ScriptPos.Line=FDebugEvalAtLine then
      FDebugLastEvalResult:=FDebugger.EvaluateAsString(FDebugEvalExpr);
end;

// EvaluateSimpleTest
//
procedure TDebuggerTests.EvaluateSimpleTest;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   expr : IdwsEvaluateExpr;
   buf : UnicodeString;
begin
   prog:=FCompiler.Compile('var i := 10;');
   try
      exec:=prog.BeginNewExecution;
      try
         exec.RunProgram(0);

         CheckEquals(10, exec.Info.ValueAsInteger['i'], 'value of i');

         expr:=TdwsCompiler.Evaluate(exec, 'i+i*10');
         try
            CheckEquals(110, expr.Expression.EvalAsInteger(exec.ExecutionObject), 'i+i*10');
         finally
            expr:=nil;
         end;

         expr:=TdwsCompiler.Evaluate(exec, 'StrToInt(''123'')');
         try
            CheckEquals(123, expr.Expression.EvalAsInteger(exec.ExecutionObject), 'StrToInt(''123'')');
         finally
            expr:=nil;
         end;

         expr:=TdwsCompiler.Evaluate(exec, 'i +* i');
         try
            expr.Expression.EvalAsString(exec.ExecutionObject, buf);
            CheckEquals('Syntax Error: Expression expected [line: 1, column: 4]'#13#10, buf, 'i +* i');
         finally
            expr:=nil;
         end;

      finally
         exec.EndProgram;
         exec:=nil;
      end;
   finally
      prog:=nil;
   end;
end;

// EvaluateOutsideOfExec
//
procedure TDebuggerTests.EvaluateOutsideOfExec;
var
   expr : IdwsEvaluateExpr;
begin
   expr:=TdwsCompiler.Evaluate(nil, 'StrToInt(''113'')+10');
   try
      CheckEquals(123, expr.Expression.EvalAsInteger(expr.Execution.ExecutionObject), 'StrToInt(''113'')+10');
   finally
      expr:=nil;
   end;
end;

// EvaluateContextTest
//
procedure TDebuggerTests.EvaluateContextTest;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile( 'var i := 1;'#13#10
                           +'procedure Test;'#13#10
                           +'var i := 2;'#13#10
                           +'begin'#13#10
                              +'PrintLn(i);'#13#10    // line 5
                           +'end;'#13#10
                           +'Test;');                 // line 7

   try
      exec:=prog.CreateNewExecution;
      try
         FDebugEvalExpr:='i';

         FDebugEvalAtLine:=5;
         FDebugLastEvalResult:='';
         FDebugger.BeginDebug(exec);
         try
            CheckEquals('2', FDebugLastEvalResult, 'i at line 5');
         finally
            FDebugger.EndDebug;
         end;

         FDebugEvalAtLine:=7;
         FDebugLastEvalResult:='';
         FDebugger.BeginDebug(exec);
         try
            CheckEquals('1', FDebugLastEvalResult, 'i at line 7');
         finally
            FDebugger.EndDebug;
         end;
      finally
         exec:=nil;
      end;
   finally
      prog:=nil;
   end;
end;

// ExecutableLines
//
procedure TDebuggerTests.ExecutableLines;
var
   prog : IdwsProgram;
   breakpointables : TdwsBreakpointableLines;

   function ReportBreakpointables : UnicodeString;
   var
      i, j : Integer;
      lines : TBits;
   begin
      Result:='';
      for i:=0 to breakpointables.Count-1 do begin
         if i>0 then
            Result:=Result+#13#10;
         Result:=Result+breakpointables.SourceName[i]+': ';
         lines:=breakpointables.SourceLines[i];
         for j:=0 to lines.Size-1 do
            if lines[j] then
               Result:=Result+IntToStr(j)+',';
      end;
   end;

begin
   prog:=FCompiler.Compile( 'var i := 1;'#13#10
                           +'procedure Test;'#13#10
                           +'var i := 2;'#13#10
                           +'begin'#13#10
                              +'PrintLn(i);'#13#10
                           +'end;'#13#10
                           +'Test;');
   CheckEquals('2'#13#10, prog.Execute.Result.ToString, 'Result 1');

   breakpointables:=TdwsBreakpointableLines.Create(prog);
   CheckEquals('*MainModule*: 1,4,5,7,', ReportBreakpointables, 'Case 1');
   breakpointables.Free;

   prog:=FCompiler.Compile( 'var i := 1;'#13#10
                           +'procedure Test;'#13#10
                           +'var i := 2;'#13#10
                           +'begin'#13#10
                              +'PrintLn(i);'#13#10
                           +'end;'#13#10
                           +'i:=i+1;');
   CheckEquals('', prog.Execute.Result.ToString, 'Result 2');

   breakpointables:=TdwsBreakpointableLines.Create(prog);
   CheckEquals('*MainModule*: 1,4,5,7,', ReportBreakpointables, 'Case 2');
   breakpointables.Free;
end;

// AttachToScript
//
procedure TDebuggerTests.AttachToScript;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile( 'var i : Integer;'#13#10
                           +'procedure Test;'#13#10
                           +'begin'#13#10
                              +'Print(Inc(i));'#13#10
                           +'end;'#13#10
                           +'Test;');

   CheckEquals('', prog.Msgs.AsInfo, 'compile');

   exec:=prog.BeginNewExecution;

   exec.RunProgram(0);
   try
      CheckEquals('1', exec.Result.ToString, 'run');

      FDebugger.AttachDebug(exec);

      CheckEquals('1', FDebugger.EvaluateAsString('i'), 'eval after attach');

      exec.Info.Func['Test'].Call;

      CheckEquals('2', FDebugger.EvaluateAsString('i'), 'eval after call');
      CheckEquals('12', exec.Result.ToString, 'result after call');

      FDebugger.DetachDebug;

      CheckEquals(DBG_NotDebugging, FDebugger.EvaluateAsString('i'), 'eval after detach');

      exec.Info.Func['Test'].Call;

      CheckEquals('123', exec.Result.ToString, 'result after re-call');
   finally
      exec.EndProgram;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TestFramework.RegisterTest('DebuggerTests', TDebuggerTests.Suite);

end.
