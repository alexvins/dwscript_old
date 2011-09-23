{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dwscript; 

interface

uses
  dwsComp, dwsCompiler, dwsCoreExprs, dwsDebugger, dwsErrors, dwsExprs, 
  dwsFileSystem, dwsFunctions, dwsGlobalVarsFunctions, dwsHtmlFilter, 
  dwsLanguageExtension, dwsMagicExprs, dwsMathFunctions, dwsRegister, 
  dwsRelExprs, dwsStack, dwsStringFunctions, dwsStringResult, dwsStrings, 
  dwsSymbols, dwsTimeFunctions, dwsTokenizer, dwsUtils, dwsVariantFunctions, 
  dwsVCLGUIFunctions, dwsXPlatform, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('dwsRegister', @dwsRegister.Register); 
end; 

initialization
  RegisterPackage('dwscript', @Register); 
end.
