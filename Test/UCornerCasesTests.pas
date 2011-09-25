unit UCornerCasesTests;

interface

uses Windows, Classes, SysUtils,
   fpcunit,testregistry,
   dws_fpcunit,
   dwsComp, dwsCompiler, dwsExprs,
   dwsTokenizer, dwsXPlatform, dwsFileSystem, dwsErrors;

type

   { TCornerCasesTests }

   TCornerCasesTests = class (TDWSTestCaseBase)
      public
         procedure DoOnInclude(const scriptName : String; var scriptSource : String);
      published
         procedure EmptyTokenBuffer;
         procedure IgnoreDecimalSeparator;
         procedure TokenizerSpecials;
         procedure TimeOutTestFinite;
         procedure TimeOutTestInfinite;
         procedure IncludeViaEvent;
         procedure IncludeViaEvent2;
         procedure IncludeViaEvent3;
         procedure IncludeViaFile;
         procedure IncludeViaFileRestricted;
         procedure StackMaxRecursion;
         procedure StackOverFlow;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

function GetTemporaryFilesPath : String;
var
   n: Integer;
begin
   SetLength(Result, MAX_PATH);
   n:=GetTempPath(MAX_PATH-1, PChar(Result));
   SetLength(Result, n);
end;



type
   // TTokenBufferWrapper
   //
   TTokenBufferWrapper = class
      Buffer : TTokenBuffer;
   end;

// ------------------
// ------------------ TCornerCasesTests ------------------
// ------------------

// EmptyTokenBuffer
//
procedure TCornerCasesTests.EmptyTokenBuffer;
var
   w : TTokenBufferWrapper;
   s : String;
begin
   w:=TTokenBufferWrapper.Create;
   try
      CheckEquals('', w.Buffer.ToStr, 'ToStr function');
      s:='dummy';
      w.Buffer.ToStr(s);
      CheckEquals('', s, 'ToStr procedure');
      s:='dummy';
      w.Buffer.ToUpperStr(s);
      CheckEquals('', s, 'ToUpperStr');
      CheckEquals(#0, w.Buffer.LastChar, 'LastChar');
   finally
      w.Free;
   end;
end;

// IgnoreDecimalSeparator
//
procedure TCornerCasesTests.IgnoreDecimalSeparator;
var
   w : TTokenBufferWrapper;
   dc : Char;
begin
   w:=TTokenBufferWrapper.Create;
   dc:=GetDecimalSeparator;
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
//
procedure TCornerCasesTests.TokenizerSpecials;
var
   t : TTokenizer;
   msgs : TdwsMessageList;
begin
   msgs:=TdwsMessageList.Create;
   t:=TTokenizer.Create('@ @= %= ^ ^=', '', msgs);
   try
      CheckTrue(t.TestDelete(ttAT), '@');
      CheckTrue(t.TestDelete(ttAT_ASSIGN), '@=');
      CheckTrue(t.TestDelete(ttPERCENT_ASSIGN), '%=');
      CheckTrue(t.TestDelete(ttCARET), '^');
      CheckTrue(t.TestDelete(ttCARET_ASSIGN), '^=');

      CheckTrue(t.TestAny([ttNAME])=ttNone, 'Any at end');
      CheckTrue(t.TestDeleteAny([ttNAME])=ttNone, 'DeleteAny at end');

   finally
      t.Free;
      msgs.Free;
   end;
end;

// TimeOutTestFinite
//
procedure TCornerCasesTests.TimeOutTestFinite;
begin
  FProg:=FCompiler.Compile('while false do;');
  FProg.TimeoutMilliseconds:=1000;
  FProg.Execute;
end;

// TimeOutTestInfinite
//
procedure TCornerCasesTests.TimeOutTestInfinite;
begin
   FProg:=FCompiler.Compile('while true do;');
   FProg.TimeoutMilliseconds:=100;
   FProg.Execute;
end;

// DoOnInclude
//
procedure TCornerCasesTests.DoOnInclude(const scriptName : String; var scriptSource : String);
begin
   CheckEquals('test.dummy', scriptName, 'DoOnInclude');
   scriptSource:='Print(''hello'');';
end;

// IncludeViaEvent
//
procedure TCornerCasesTests.IncludeViaEvent;
begin
   FCompiler.OnInclude:=nil;
   FCompiler.Config.ScriptPaths.Clear;
   Compile('{$include}');
   CheckEqualsInfo('Syntax Error: Name of include file expected [line: 1, column: 10]'#13#10,
                   'include missing');
end;

procedure TCornerCasesTests.IncludeViaEvent2;
begin
  FCompiler.OnInclude:=nil;
  FCompiler.Config.ScriptPaths.Clear;
  Compile('{$include ''test.dummy''}');
  CheckEqualsInfo('Compile Error: Couldn''t find file "test.dummy" on input paths [line: 1, column: 11]'#13#10,
                  'include forbidden');
end;

procedure TCornerCasesTests.IncludeViaEvent3;
begin
  FCompiler.Config.ScriptPaths.Clear;
  FCompiler.OnInclude:=@DoOnInclude;
  Compile('{$include ''test.dummy''}');
  CheckEmptyInfo('include via event');
  Execute;
  CheckEqualsResult('hello', 'exec include via event');
end;

// IncludeViaFile
//
procedure TCornerCasesTests.IncludeViaFile;
var
   prog : TdwsProgram;
   sl : TStringList;
   tempDir : String;
   tempFile : String;
begin
   FCompiler.OnInclude:=nil;

   tempDir:=GetTemporaryFilesPath;
   tempFile:=tempDir+'test.dummy';

   sl:=TStringList.Create;
   try
      sl.Add('Print(''world'');');
      sl.SaveToFile(tempFile);
   finally
      sl.Free;
   end;

   FCompiler.Config.ScriptPaths.Clear;
   prog:=FCompiler.Compile('{$include ''test.dummy''}');
   try
      CheckEquals('Compile Error: Couldn''t find file "test.dummy" on input paths [line: 1, column: 11]'#13#10,
                  prog.Msgs.AsInfo, 'include via file no paths');
   finally
      prog.Free;
   end;

   FCompiler.Config.ScriptPaths.Add(tempDir);
   prog:=FCompiler.Compile('{$include ''test.dummy''}');
   try
      CheckEquals('', prog.Msgs.AsInfo, 'include via file');
      prog.Execute;
      CheckEquals('world', (prog.Result as TdwsDefaultResult).Text, 'exec include via file');
   finally
      prog.Free;
   end;

   FCompiler.Config.ScriptPaths.Clear;
   DeleteFile(tempFile);
end;

// IncludeViaFileRestricted
//
procedure TCornerCasesTests.IncludeViaFileRestricted;

   function GetTemporaryFilesPath : String;
   var
      n: Integer;
   begin
      SetLength(Result, MAX_PATH);
      n:=GetTempPath(MAX_PATH-1, PChar(Result));
      SetLength(Result, n);
   end;

var
   prog : TdwsProgram;
   sl : TStringList;
   tempDir : String;
   tempFile : String;
   restricted : TdwsRestrictedFileSystem;
begin
   restricted:=TdwsRestrictedFileSystem.Create(nil);
   FCompiler.OnInclude:=nil;
   FCompiler.Config.CompileFileSystem:=restricted;

   tempDir:=GetTemporaryFilesPath;
   tempFile:=tempDir+'test.dummy';

   sl:=TStringList.Create;
   try
      sl.Add('Print(''world'');');
      sl.SaveToFile(tempFile);
   finally
      sl.Free;
   end;

   restricted.Paths.Text:=tempDir+'\nothing';
   prog:=FCompiler.Compile('{$include ''test.dummy''}');
   try
      CheckEquals('Compile Error: Couldn''t find file "test.dummy" on input paths [line: 1, column: 11]'#13#10,
                  prog.Msgs.AsInfo, 'include via file no paths');
   finally
      prog.Free;
   end;

   restricted.Paths.Text:=tempDir;

   prog:=FCompiler.Compile('{$include ''test.dummy''}');
   try
      CheckEquals('Compile Error: Couldn''t find file "test.dummy" on input paths [line: 1, column: 11]'#13#10,
                  prog.Msgs.AsInfo, 'include via file restricted - no paths');
   finally
      prog.Free;
   end;

   FCompiler.Config.ScriptPaths.Add('.');
   prog:=FCompiler.Compile('{$include ''test.dummy''}');
   try
      CheckEquals('', prog.Msgs.AsInfo, 'include via file restricted - dot path');
      prog.Execute;
      CheckEquals('world', (prog.Result as TdwsDefaultResult).Text, 'exec include via file');
   finally
      prog.Free;
   end;

   DeleteFile(tempFile);
   restricted.Free;

   CheckTrue(FCompiler.Config.CompileFileSystem=nil, 'Notification release');
end;

// StackMaxRecursion
//
procedure TCornerCasesTests.StackMaxRecursion;
var
   prog : TdwsProgram;
begin
   FCompiler.Config.MaxRecursionDepth:=20;

   prog:=FCompiler.Compile('procedure Dummy; begin Dummy; end; Dummy;');
   try
      CheckEquals('', prog.Msgs.AsInfo, 'compile');
      prog.Execute;
      CheckEquals('Runtime Error: Maximal recursion exceeded (20 calls)'#13#10,
                  prog.Msgs.AsInfo, 'stack max recursion');
   finally
      prog.Free;
   end;

   FCompiler.Config.MaxDataSize:=cDefaultMaxRecursionDepth;
end;

// StackOverFlow
//
procedure TCornerCasesTests.StackOverFlow;
var
   prog : TdwsProgram;
begin
   FCompiler.Config.MaxDataSize:=1024;

   prog:=FCompiler.Compile('procedure Dummy; var i : Integer; begin Dummy; end; Dummy;');
   try
      CheckEquals('', prog.Msgs.AsInfo, 'compile');
      prog.Execute;
      CheckEquals('Runtime Error: Maximal data size exceeded (64 Variants)'#13#10,
                  prog.Msgs.AsInfo, 'stack overflow');
   finally
      prog.Free;
   end;

   FCompiler.Config.MaxDataSize:=0;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('CornerCasesTests', TCornerCasesTests.Suite);

end.
