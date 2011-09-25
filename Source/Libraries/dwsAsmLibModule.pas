{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
{$I dws.inc}
unit dwsAsmLibModule;

interface

uses Windows, Classes, SysUtils, dwsLanguageExtension, dwsComp, dwsCompiler,
   dwsExprs, dwsTokenizer, dwsSymbols, dwsErrors, dwsCoreExprs, dwsStack,
   dwsStrings, dwsXPlatform;

type

   // TdwsASMLibModule
   //
   TdwsASMLibModule = class (TdwsCustomLangageExtension)
      private

      protected
         function CreateExtension : TdwsLanguageExtension; override;

      public
   end;

   // TdwsAsmLanguageExtension
   //
   TdwsAsmLanguageExtension = class (TdwsLanguageExtension)
      protected
         function SymbolDefines(const symbolName : String; compiler : TdwsCompiler) : String;
         function AssembleViaNASM(const code : TStrings; const basePos : TScriptPos; msgs : TdwsMessageList) : TBytes;
      public
         function ReadInstr(compiler : TdwsCompiler) : TNoResultExpr; override;
   end;

   // TdwsASMBlockExpr
   //
   TdwsASMBlockExpr = class (TNoResultExpr)
      private
         FCodePtr : Pointer;
         FCodeSize : Integer;
      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; const binary : TBytes);
         destructor Destroy; override;
         procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsASMLibModule ------------------
// ------------------

// CreateExtension
//
function TdwsASMLibModule.CreateExtension : TdwsLanguageExtension;
begin
   Result:=TdwsAsmLanguageExtension.Create;
end;

// ------------------
// ------------------ TdwsAsmLanguageExtension ------------------
// ------------------

// ReadInstr
//
function TdwsAsmLanguageExtension.ReadInstr(compiler : TdwsCompiler) : TNoResultExpr;
var
   i : Integer;
   tok : TTokenizer;
   token : TTokenType;
   startPos, curPos : PChar;
   hotPos, asmPos : TScriptPos;
   curInstr, curDefines : String;
   outputAsm, nameSymbols : TStringList;
begin
   Result:=nil;
   tok:=compiler.Tokenizer;

   if not (tok.TestName and SameText(tok.GetToken.FString, 'asm')) then Exit;

   hotPos:=tok.HotPos;
   tok.KillToken;
   startPos:=tok.PosPtr;
   tok.TestName;
   asmPos:=tok.HotPos;

   outputAsm:=TStringList.Create;
   nameSymbols:=TStringList.Create;
   try
      nameSymbols.Sorted:=True;
      nameSymbols.Duplicates:=dupIgnore;

      outputAsm.Add('BITS 32');

      outputAsm.Add('push ebp');
      outputAsm.Add('mov ebp, eax');

      // collect everything until 'end'
      while tok.HasTokens do begin

         token:=tok.TestAny([ttNAME, ttSEMI, ttCOLON, ttEND]);
         case token of
            ttNone :
               tok.KillToken;
            ttNAME : begin
               nameSymbols.Add(tok.GetToken.FString);
               tok.KillToken;
            end;
            ttSEMI, ttCOLON : begin
               curPos:=tok.PosPtr;
               SetString(curInstr, startPos, (NativeUInt(curPos)-NativeUInt(startPos)) div SizeOf(Char)-1);
               curInstr:=Trim(curInstr);
               if LastDelimiter(#13#10, curInstr)>0 then
                  compiler.CurrentProg.Msgs.AddCompilerStop(tok.HotPos, CPE_SemiExpected);

               case token of
                  ttSEMI : begin
                     outputAsm.AddObject(curInstr, TObject(tok.HotPos.Line));
                  end;
                  ttCOLON : begin
                     outputAsm.AddObject(curInstr+':', TObject(tok.HotPos.Line));
                  end;
               else
                  Assert(False);
               end;

               tok.KillToken;
               startPos:=tok.PosPtr;
               tok.TestName;
               asmPos:=tok.HotPos;
            end;
            ttEND : begin
               SetString(curInstr, startPos, (NativeUInt(tok.PosPtr)-NativeUInt(startPos)) div SizeOf(Char)-1);
               curInstr:=Trim(curInstr);
               if LastDelimiter(#13#10, curInstr)>0 then
                  compiler.CurrentProg.Msgs.AddCompilerStop(asmPos, CPE_SemiExpected);
               tok.KillToken;
               Break;
            end;
         else
            Assert(False);
         end;

      end;

      outputAsm.Add('pop ebp');
      outputAsm.Add('ret');

      if not tok.HasTokens then begin
         compiler.CurrentProg.Msgs.AddErrorStop('Incomplete asm block');
         Exit;
      end;

      // generate defines for encountered symbols

      for i:=nameSymbols.Count-1 downto 0 do begin
         curDefines:=SymbolDefines(nameSymbols[i], compiler);
         if curDefines<>'' then
            outputAsm.Insert(1, curDefines);
      end;

      // assemble via NASM

      Result:=TdwsASMBlockExpr.Create(compiler.CurrentProg, hotPos,
                                      AssembleViaNASM(outputAsm, hotPos, compiler.CurrentProg.Msgs));

   finally
      nameSymbols.Free;
      outputAsm.Free;
   end;
end;


// AssembleViaNASM
//
function TdwsAsmLanguageExtension.AssembleViaNASM(const code : TStrings;
      const basePos : TScriptPos; msgs : TdwsMessageList) : TBytes;
var
   i, p, k : Integer;
   tempFileNameAsm, tempFileNameBin, tempFileNameErr : String;
   commandLine, errorLine : String;
   waitResult  : Cardinal;
   startupInfo : TStartupInfo;
   processInfo : TProcessInformation;
   pCurrentDirectory : PChar;
   errors : TStringList;

   script_pos: TScriptPos;
begin
   tempFileNameAsm:=TPath.GetTempFileName;
   tempFileNameBin:=tempFileNameAsm+'.bin';
   tempFileNameErr:=tempFileNameAsm+'.err';
   try
      commandLine:='nasm -Xvc -f bin -o "'+tempFileNameBin+'" -Z "'+tempFileNameErr+'" "'+tempFileNameAsm+'"';
      code.SaveToFile(tempFileNameAsm);

      FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
      with StartupInfo do begin
         cb:=SizeOf(TStartupInfo);
         dwFlags:=(STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK);
         wShowWindow:=SW_HIDE;
      end;

      pCurrentDirectory:=PChar(ExtractFilePath(ParamStr(0)));
      if CreateProcess(nil, PChar(commandLine), nil, nil, False, NORMAL_PRIORITY_CLASS, nil,
                       pCurrentDirectory, startupInfo, processInfo) then begin
         try
            // timeout is in milliseconds or INFINITE if you want to wait forever
            waitResult:=WaitForSingleObject(ProcessInfo.hProcess, 5000);
            if waitResult<>WAIT_OBJECT_0 then
               msgs.AddErrorStop('NASM call timed out');

            errors:=TStringList.Create;
            try
               errors.LoadFromFile(tempFileNameErr);
               for i:=0 to errors.Count-1 do begin
                  errorLine:=errors[i];
                  p:=Pos('(', errorLine);
                  if p>0 then begin
                     Delete(errorLine, 1, p);
                     p:=Pos(')', errorLine);
                     k:=StrToInt(Copy(errorLine, 1, p-1));
                     Delete(errorLine, 1, p+2);
                     script_pos := TScriptPos.Create(basePos.SourceFile, Integer(code.Objects[k-1]), 1);
                     msgs.AddCompilerError(script_pos,'asm '+errorLine);
                  end else msgs.AddError(errorLine);
               end;
               if errors.Count>0 then
                  Exit;
            finally
               errors.Free;
            end;

            Result:=TFile.ReadAllBytes(tempFileNameBin);

         finally
            CloseHandle(ProcessInfo.hThread);
            CloseHandle(ProcessInfo.hProcess);
         end;
      end else begin
         msgs.AddErrorStop('NASM call failed out '+IntToStr(GetLastError));
      end;

   finally
      DeleteFile(tempFileNameAsm);
      DeleteFile(tempFileNameBin);
      DeleteFile(tempFileNameErr);
   end;
end;

// SymbolDefines
//
function TdwsAsmLanguageExtension.SymbolDefines(const symbolName : String; compiler : TdwsCompiler) : String;
var
   offset : Integer;
   sym : TSymbol;
   dataSym : TDataSymbol;
   size, sign : String;
begin
   sym:=compiler.CurrentProg.Table.FindSymbol(symbolName);
   if not Assigned(sym) then Exit('');

   case sym.BaseTypeID of
      typIntegerID, typFloatID, typStringID : size:='QWORD';
      typBooleanID : size:='WORD';
   else
      Exit('');
   end;

   if sym is TConstSymbol then begin

      Result:=Format( '%%define %0:s %1:s [0x%2:x]'#13#10
                     +'%%define @%0:s 0x%2:x',
                     [sym.Name, size, NativeUInt(@(TConstSymbol(sym).Data[0]))+$8]);

   end else if sym is TDataSymbol then begin

      if sym is TByRefParamSymbol then Exit('');

      dataSym:=TDataSymbol(sym);
      if dataSym.Level<>compiler.CurrentProg.Level then Exit('');

      offset:=(dataSym.StackAddr-compiler.CurrentProg.Stack.BasePointer)*SizeOf(Variant)+$8;
      if offset>0 then
         sign:='+'
      else sign:='';
      if size<>'' then
         Result:=Format( '%%define %0:s %2:s [EBP%3:s%1:d]'#13#10
                        +'%%define @%0:s EBP%3:s%1:d',
                        [sym.Name, offset, size, sign])
      else Result:='';

   end;
end;

// ------------------
// ------------------ TdwsASMBlockExpr ------------------
// ------------------

// Create
//
constructor TdwsASMBlockExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; const binary : TBytes);
begin
   inherited Create(Prog, Pos);
   FCodeSize:=Length(binary);
   FCodePtr:=VirtualAlloc(nil, FCodeSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
   System.Move(binary[0], FCodePtr^, FCodeSize);
end;

// Destroy
//
destructor TdwsASMBlockExpr.Destroy;
begin
   inherited;
   VirtualFree(FCodePtr, FCodeSize, MEM_RELEASE);
end;

// EvalNoResult
//
procedure TdwsASMBlockExpr.EvalNoResult(var status : TExecutionStatusResult);
type
   TJumpFunc = procedure(stack : Pointer);
var
   stack : TStack;
begin
   stack:=Prog.Stack;
   TJumpFunc(FCodePtr)(@stack.Data[stack.BasePointer]);
end;

end.
