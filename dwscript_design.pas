{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dwscript_design; 

interface

uses
  dwsRegister, dwsExperts, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('dwsRegister', @dwsRegister.Register); 
  RegisterUnit('dwsExperts', @dwsExperts.Register); 
end; 

initialization
  RegisterPackage('dwscript_design', @Register); 
end.
