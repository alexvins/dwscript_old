{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dwscript_classeslib; 

interface

uses
  dwsClasses, dwsClassesLibModule, dwsHashtables, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('dwsClassesLibModule', @dwsClassesLibModule.Register); 
end; 

initialization
  RegisterPackage('dwscript_classeslib', @Register); 
end.
