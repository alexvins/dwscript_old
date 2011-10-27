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

interface

uses
   Classes, SysUtils,
   {$ifdef FPC}
   fpcunit, testutils, testregistry
   {$else}
   TestFrameWork
   {$endif}
   ;

type

   {$ifdef FPC}

   __TTestCase = fpcunit.TTestCase;

   { TTestCase }

   TTestCase = class(__TTestCase)
   public
     procedure CheckException(AMethod: TRunMethod; AExceptionClass: ExceptClass; msg :string = '');
   end;

   {$else}
   TTestCase = TestFrameWork.TTestCase;
   {$endif}

procedure RegisterTest(const testName : UnicodeString; aTest : TTestCaseClass);

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

{ TTestCase }

procedure TTestCase.CheckException(AMethod: TRunMethod;
  AExceptionClass: ExceptClass; msg: string);
begin
  inherited AssertException(msg, AExceptionClass, AMethod);
end;

end.

