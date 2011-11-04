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
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsMathComplexFunctions;

{$I dws.inc}

interface

uses dwsFunctions, dwsSymbols, dwsExprs, dwsStrings, dwsOperators, dwsStack,
   dwsTokenizer, SysUtils, dwsUtils, dwsXPlatform;

type
   TComplexMakeExpr = class(TInternalMagicDataFunction)
      public
         procedure DoEval(args : TExprBaseList; var result : TDataPtr); override;
   end;

   TComplexToStrExpr = class(TInternalMagicStringFunction)
      public
         procedure DoEvalAsString(args : TExprBaseList; var Result : UnicodeString); override;
   end;

   TComplexBinOpExpr = class(TInternalMagicDataFunction);

   TComplexAddOpExpr = class(TComplexBinOpExpr)
      public
         procedure DoEval(args : TExprBaseList; var result : TDataPtr); override;
   end;

   TComplexSubOpExpr = class(TComplexBinOpExpr)
      public
         procedure DoEval(args : TExprBaseList; var result : TDataPtr); override;
   end;

   TComplexMultOpExpr = class(TComplexBinOpExpr)
      public
         procedure DoEval(args : TExprBaseList; var result : TDataPtr); override;
   end;

const
   SYS_COMPLEX = 'TComplex';

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

//type
//   TComplexProgram = class helper for TdwsProgram
//      function TypComplex : TRecordSymbol;
//   end;
//
//// TypComplex
////
//function TComplexProgram.TypComplex : TRecordSymbol;
//begin
//   Result:=SystemTable.FindTypeSymbol(SYS_COMPLEX, cvMagic) as TRecordSymbol;
//end;

// RegisterComplexType
//
procedure RegisterComplexType(systemTable : TSymbolTable; unitSyms : TUnitMainSymbols;
                              unitTable : TSymbolTable; operators : TOperators);
var
   typComplex : TRecordSymbol;
   typFloat : TBaseFloatSymbol;
begin
   typFloat:=SystemTable.FindSymbol(SYS_FLOAT, cvMagic) as TBaseFloatSymbol;

   typComplex:=TRecordSymbol.Create(SYS_COMPLEX, nil);
   typComplex.AddField(TFieldSymbol.Create('Re', typFloat, cvPublic));
   typComplex.AddField(TFieldSymbol.Create('Im', typFloat, cvPublic));

   systemTable.AddSymbol(typComplex);
end;

// RegisterComplexOperators
//
procedure RegisterComplexOperators(systemTable : TSymbolTable; unitSyms : TUnitMainSymbols;
                                   unitTable : TSymbolTable; operators : TOperators);
var
   typComplex : TRecordSymbol;
begin
   typComplex:=systemTable.FindTypeSymbol(SYS_COMPLEX, cvMagic) as TRecordSymbol;

   operators.RegisterOperator(ttPLUS, unitTable.FindSymbol('ComplexAdd', cvMagic) as TFuncSymbol, typComplex, typComplex);
   operators.RegisterOperator(ttMINUS, unitTable.FindSymbol('ComplexSub', cvMagic) as TFuncSymbol, typComplex, typComplex);
   operators.RegisterOperator(ttTIMES, unitTable.FindSymbol('ComplexMult', cvMagic) as TFuncSymbol, typComplex, typComplex);
end;

// ------------------
// ------------------ TComplexMakeExpr ------------------
// ------------------

// DoEval
//
procedure TComplexMakeExpr.DoEval(args : TExprBaseList; var result : TDataPtr);
begin
   result[0]:=args.AsFloat[0];
   result[1]:=args.AsFloat[1];
end;

// ------------------
// ------------------ TComplexToStrExpr ------------------
// ------------------

// DoEvalAsString
//
procedure TComplexToStrExpr.DoEvalAsString(args : TExprBaseList; var Result : UnicodeString);
var
   cmplxData : TDataPtr;
   r, i : Double;
begin
   cmplxData:=TDataExpr(args.ExprBase[0]).DataPtr[args.Exec];
   r:=cmplxData[0];
   i:=cmplxData[1];
   if i>0 then
      Result:=dwsFormat('%f + %fi', [r, i])
   else if i<0 then
      Result:=dwsFormat('%f - %fi', [r, Abs(i)])
   else Result:=dwsFormat('%f', [r]);
end;

// ------------------
// ------------------ TComplexAddOpExpr ------------------
// ------------------

// DoEval
//
procedure TComplexAddOpExpr.DoEval(args : TExprBaseList; var result : TDataPtr);
var
   leftData, rightData : TDataPtr;
begin
   leftData:=TDataExpr(args.ExprBase[0]).DataPtr[args.Exec];
   rightData:=TDataExpr(args.ExprBase[1]).DataPtr[args.Exec];

   result[0]:=leftData[0]+rightData[0];
   result[1]:=leftData[1]+rightData[1];
end;

// ------------------
// ------------------ TComplexSubOpExpr ------------------
// ------------------

// DoEval
//
procedure TComplexSubOpExpr.DoEval(args : TExprBaseList; var result : TDataPtr);
var
   leftData, rightData : TDataPtr;
begin
   leftData:=TDataExpr(args.ExprBase[0]).DataPtr[args.Exec];
   rightData:=TDataExpr(args.ExprBase[1]).DataPtr[args.Exec];

   result[0]:=leftData[0]-rightData[0];
   result[1]:=leftData[1]-rightData[1];
end;

// ------------------
// ------------------ TComplexMultOpExpr ------------------
// ------------------

// DoEval
//
procedure TComplexMultOpExpr.DoEval(args : TExprBaseList; var result : TDataPtr);
var
   leftData, rightData : TDataPtr;
begin
   leftData:=TDataExpr(args.ExprBase[0]).DataPtr[args.Exec];
   rightData:=TDataExpr(args.ExprBase[1]).DataPtr[args.Exec];

   result[0]:=leftData[0]*rightData[0]-leftData[1]*rightData[1];
   result[1]:=leftData[1]*rightData[0]+leftData[0]*rightData[1];
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   dwsInternalUnit.AddPreInitProc(RegisterComplexType);
   dwsInternalUnit.AddPostInitProc(RegisterComplexOperators);

   RegisterInternalFunction(TComplexMakeExpr, 'Complex', ['real', SYS_FLOAT, 'imaginary', SYS_FLOAT], SYS_COMPLEX, True);
   RegisterInternalStringFunction(TComplexToStrExpr, 'ComplexToStr', ['c', SYS_COMPLEX], True);

   RegisterInternalFunction(TComplexAddOpExpr, 'ComplexAdd', ['left', SYS_COMPLEX, 'right', SYS_COMPLEX], SYS_COMPLEX, True);
   RegisterInternalFunction(TComplexSubOpExpr, 'ComplexSub', ['left', SYS_COMPLEX, 'right', SYS_COMPLEX], SYS_COMPLEX, True);
   RegisterInternalFunction(TComplexMultOpExpr, 'ComplexMult', ['left', SYS_COMPLEX, 'right', SYS_COMPLEX], SYS_COMPLEX, True);

end.