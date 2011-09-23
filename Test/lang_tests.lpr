program lang_tests;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  TextTestRunner,
  Classes, SysUtils,
  UdwsUnitTests, //ok
  UdwsUtilsTests,//ok
  UHTMLFilterTests,
  //UMemoryTests,
  //UScriptTests,
  //UTestDispatcher,
  //UAlgorithmsTests,
  //UCornerCasesTests,
  //UdwsFunctionsTests,
  CustApp
  { you can add units after this };

type

  { TDWScriptTests }

  TDWScriptTests = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TDWScriptTests }

procedure TDWScriptTests.DoRun;
begin
  try
    RunRegisteredTests;
  finally
   Terminate;
  end;

end;

constructor TDWScriptTests.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TDWScriptTests.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TDWScriptTests;

{$R *.res}

begin
  Application:=TDWScriptTests.Create(nil);
  Application.Run;
  Application.Free;
end.

