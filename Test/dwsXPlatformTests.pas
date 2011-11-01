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
{    Copyright Eric Grange / Creative IT                               }
{                                                                      }
{**********************************************************************}
unit dwsXPlatformTests;

{.$DEFINE DWS_USE_DUNIT}

{$IFNDEF FPC}
  {$DEFINE DWS_USE_DUNIT} //in Delphi use DUnit always
{$ENDIF}

interface

uses
   Classes, SysUtils,
   {$ifdef DWS_USE_DUNIT}
   TestFrameWork //DUnit in Delphi or FPtest in FPC. They are compatible
   {$else}
   fpcunit, testutils, testregistry
   {$endif}

   {$IFDEF FPC}
    ,LazUtils, FileUtil,lazutf8classes
   {$ENDIF}
   ;

type

   {$ifndef DWS_USE_DUNIT}

   __TTestCase = fpcunit.TTestCase; //ugly but TTestCase = class(fpcunit.TTestCase) does not compiles

   { TTestCase }

   TTestCase = class(__TTestCase)
   public
     //this method is unimplemented in fpcunit
     procedure CheckException(AMethod: TRunMethod; AExceptionClass: ExceptClass; msg :string = '');
   end;

   {$else}
   TTestCase = TestFrameWork.TTestCase;
   {$endif}

procedure RegisterTest(const testName : UnicodeString; aTest : TTestCaseClass); overload;
function LoadScriptSource(const FileName: UnicodeString):UnicodeString;

{$IFDEF FPC}
procedure RegisterTest(ASuitePath: String; ATest: TTest); overload;
{$ENDIF}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// RegisterTest
//
procedure RegisterTest(const testName : UnicodeString; aTest : TTestCaseClass);
begin
   {$ifdef FPC}
   testregistry.RegisterTest(aTest);
   {$else}
   TestFrameWork.RegisterTest(testName, aTest.Suite);
   {$endif}
end;

function LoadScriptSource(const FileName: UnicodeString): UnicodeString;
{$IFDEF FPC}
var
  source : TStringListUTF8;
begin
   source := TStringListUTF8.Create;
   try
      source.LoadFromFile(UTF8Encode(FileName));
      Result := UTF8Decode(source.Text);
   finally
      source.Free;
   end;
{$ELSE}
var
   source : TStringList;
begin
   source := TStringList.Create;
   try
      source.LoadFromFile(FileName);
      Result := sourse.Text;
   finally
      source.Free;
   end;
{$ENDIF}
end;

{$IFDEF FPC}
procedure RegisterTest(ASuitePath: String; ATest: TTest);
begin
  testregistry.RegisterTest(ASuitePath, ATest);
end;
{$ENDIF}

{ TTestCase }

procedure TTestCase.CheckException(AMethod: TRunMethod;
  AExceptionClass: ExceptClass; msg: string);
begin
  inherited AssertException(msg, AExceptionClass, AMethod);
end;

end.

