program lang_tests2;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, dws_fpcunit, UTestDispatcher,
  UAlgorithmsTests, UCornerCasesTests, UdwsFunctionsTests, UdwsUnitTests,
  UdwsUtilsTests, UHTMLFilterTests, UMemoryTests, UScriptTests;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

