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
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. For other initial contributors, see contributors.txt   }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
{$I dws.inc}
unit dwsRegister;

interface

procedure Register;

implementation

uses
  Classes, Controls, dwsComp, dwsComConnector, dwsDebugger, dwsGlobalVarsFunctions,
  dwsVCLGUIFunctions, dwsHtmlFilter, dwsClasses, dwsClassesLibModule, dwsFileSystem;

procedure Register;
begin
  RegisterClass(TdwsComConnector);

  RegisterComponents('dws',
                     [
                     TDelphiWebScript,
                     TdwsComConnector,
                     TdwsSimpleDebugger,
                     TdwsUnit,
                     TdwsHtmlFilter,
                     TdwsGlobalVarsFunctions,
                     TdwsGUIFunctions,
                     TdwsRestrictedFileSystem,
                     TdwsNoFileSystem
                     ]);
end;

end.

