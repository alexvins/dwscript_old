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
{    The Initial Developer of the Original DWS Code is Matthias        }
{    Ackermann.                                                        }
{    For other initial contributors, see DWS contributors.txt          }
{    Ackermann.                                                        }
{    DWS code is currently maintained by Eric Grange.                  }
{                                                                      }
{    Current maintainer of the IDE utility: Brian Frost                }
{                                                                      }
{**********************************************************************}
unit UDwsIdeDefs;

interface

uses
  Windows,
  Graphics,
  dwsDebugger,
  SynEditHighlighter,
  SynHighlighterDWS;

type

  IDwsIde = interface
    // IDwsIde
    // -------------------------------------------------------------------------
    function  DwsIde_GetDebugger : TdwsDebugger;
    // -------------------------------------------------------------------------
  end;

  TEditorHighlighterClass = class of TSynCustomHighlighter;

  TDwsIdeOptions = record
    EditorHighlighterClass : TEditorHighlighterClass;
    EditorFontName         : string;
    EditorFontSize         : integer;
  end;


const
  IdeOptions_Style1 : TDwsIdeOptions = (
    EditorHighlighterClass : TSynDWSSyn;
    EditorFontName         : 'Lucida sans typewriter';
    EditorFontSize         : 10
    );


function DebuggerEvaluate( ADebugger : TDwsDebugger; const AExpression : string) : String;

function Lighten( AColor: TColor; AFactor: Byte): TColor;
// Lightens a color by this amount

implementation

uses
  SysUtils,
  variants,
  dwsCompiler;


function DebuggerEvaluate( ADebugger : TDwsDebugger; const AExpression : string) : String;
var
  expr : IdwsEvaluateExpr;
  V    : variant;
begin
   try
      expr:= ADebugger.Evaluate(AExpression);
      try
         Result:='(no result)';
         expr.Expression.EvalAsVariant( ADebugger.Execution.ExecutionObject, V );
         Result := VarToStr( V );
         if VarIsStr( V ) then
           Result := '''' + Result + '''';
      finally
         expr:=nil;
      end;
   except
      on E : Exception do
         Result:=E.Message;
   end;
end;



function Lighten( AColor: TColor; AFactor: Byte): TColor;
// Lightens a color by this amount
var
  R, G, B: Byte;
begin
  AColor := ColorToRGB( AColor );

  R := GetRValue(AColor);
  G := GetGValue(AColor);
  B := GetBValue(AColor);

  Inc( R, AFactor );
  Inc( G, AFactor );
  Inc( B, AFactor );

  Result := RGB(R, G, B);
end;



end.
