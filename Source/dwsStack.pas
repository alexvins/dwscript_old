{**********************************************************************}

 {    "The contents of this file are subject to the Mozilla Public      }
 {    License Version 1.1 (the "License"); you may not use this         }
 {    file except in compliance with the License. You may obtain        }
 {    a copy of the License at http://www.mozilla.org/MPL/              }

 {    Software distributed under the License is distributed on an       }
 {    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
 {    or implied. See the License for the specific language             }
 {    governing rights and limitations under the License.               }

 {    The Initial Developer of the Original Code is Matthias            }
 {    Ackermann. For other initial contributors, see contributors.txt   }
 {    Subsequent portions Copyright Creative IT.                        }

{    Current maintainer: Eric Grange                                   }

{**********************************************************************}

{$ifdef FPC}
  {$MODE OBJFPC}
  {$T-}
  {$A+}
  {$H+}
{.$inline off}
{$ENDIF}


{.$I dws.inc}

unit dwsStack;

interface

uses Variants, Classes, SysUtils, dwsStrings, dwsUtils, dwsXPlatform;

type

  TData = array of Variant;
  PData = ^TData;

  // TStack

  TStack = class
  private
    FBasePointer: Integer;
    FBpStore: array of TSimpleStackInteger;
    FChunkSize: Integer;
    FMaxLevel: Integer;
    FMaxSize: Integer;
    FSize: Integer;
    FStackPointer: Integer;
    FRecursionDepth: Integer;
    FMaxRecursionDepth: Integer;

    function GetFrameSize: Integer;

    procedure ClearBpStore;

    procedure GrowTo(desiredSize: Integer);

  public
    Data: TData;

    constructor Create(chunkSize, maxByteSize: Integer;
      maxRecursionDepth: Integer);
    destructor Destroy; override;

    function NextLevel(Level: Integer): Integer;

    procedure Push(Delta: Integer);
    procedure Pop(Delta: Integer);

    procedure IncRecursion; inline;
    procedure DecRecursion; inline;

    procedure WriteData(SourceAddr, DestAddr, Size: Integer;
      const sourceData: TData);
    procedure ReadData(SourceAddr, DestAddr, Size: Integer; DestData: TData);
    procedure CopyData(SourceAddr, DestAddr, Size: Integer);

    procedure WriteValue(DestAddr: Integer; const Value: Variant); inline;
    procedure WriteIntValue(DestAddr: Integer; const Value: Int64);
      overload; inline;
    procedure WriteIntValue(DestAddr: Integer; const pValue: PInt64);
      overload; inline;
    procedure WriteFloatValue(DestAddr: Integer; var Value: Double); inline;
    procedure WriteStrValue(DestAddr: Integer; const Value: string); inline;
    procedure WriteBoolValue(DestAddr: Integer; const Value: Boolean); inline;
    procedure WriteInterfaceValue(DestAddr: Integer; const intf: IUnknown); inline;

    function SetStrChar(DestAddr: Integer; index: Integer; c: Char): Boolean;

    function ReadValue(SourceAddr: Integer): Variant; inline;
    function ReadIntValue(SourceAddr: Integer): Int64; inline;
    procedure ReadIntAsFloatValue(SourceAddr: Integer; var Result: Double); inline;
    procedure ReadFloatValue(SourceAddr: Integer; var Result: Double);
    procedure ReadStrValue(SourceAddr: Integer; var Result: string);
    function ReadBoolValue(SourceAddr: Integer): Boolean;
    procedure ReadInterfaceValue(SourceAddr: Integer; var Result: IUnknown);

    function PointerToIntValue(addr: Integer): PInt64;
    function PointerToFloatValue(addr: Integer): PDouble;

    procedure IncIntValue(destAddr: Integer; const Value: Int64); inline;
    procedure AppendStringValue(destAddr: Integer; const Value: string);

    procedure PushBp(Level, Bp: Integer);
    function GetSavedBp(Level: Integer): Integer;
    function PopBp(Level: Integer): Integer;

    procedure SwitchFrame(var oldBasePointer: Integer);
    procedure RestoreFrame(oldBasePointer: Integer);
    procedure Reset;

    property BasePointer: Integer read FBasePointer write FBasePointer;
    property FrameSize: Integer read GetFrameSize;
    property MaxSize: Integer read FMaxSize write FMaxSize;
    property StackPointer: Integer read FStackPointer;
    property RecursionDepth: Integer read FRecursionDepth;
    property MaxRecursionDepth: Integer
      read FMaxRecursionDepth write FMaxRecursionDepth;
  end;

procedure CopyData(const SourceData: TData; SourceAddr: Integer;
  DestData: TData; DestAddr: Integer; Size: Integer);

 // ------------------------------------------------------------------
 // ------------------------------------------------------------------
 // ------------------------------------------------------------------
implementation
 // ------------------------------------------------------------------
 // ------------------------------------------------------------------
 // ------------------------------------------------------------------

uses dwsErrors;

// CopyData

procedure CopyData(const SourceData: TData; SourceAddr: Integer;
  DestData: TData; DestAddr: Integer; Size: Integer);
begin
  while Size > 0 do
  begin
    VarCopy(DestData[DestAddr], SourceData[SourceAddr]);
    Inc(SourceAddr);
    Inc(DestAddr);
    Dec(Size);
  end;
end;

//// FallBack_VarDataToInt64
//
//procedure FallBack_VarDataToInt64(varData: PVarData);
//begin
//
//end;

 // ------------------
 // ------------------ TStack ------------------
 // ------------------

constructor TStack.Create(chunkSize, maxByteSize: Integer; maxRecursionDepth: Integer);
begin
  FChunkSize := chunkSize;
  FMaxSize   := maxByteSize div SizeOf(Variant);
  FMaxRecursionDepth := maxRecursionDepth;
  FMaxLevel  := 1;
end;

// Destroy

destructor TStack.Destroy;
begin
  inherited;
  ClearBpStore;
end;

// ClearBpStore

procedure TStack.ClearBpStore;
var
  i: Integer;
begin
  for i := 0 to High(FBpStore) do
  begin
    FBpStore[i].Free;
  end;
end;

procedure TStack.CopyData(SourceAddr, DestAddr, Size: Integer);
begin
  while Size > 0 do
  begin
    VarCopy(Data[DestAddr], Data[SourceAddr]);
    Inc(SourceAddr);
    Inc(DestAddr);
    Dec(Size);
  end;
end;

function TStack.GetFrameSize: Integer;
begin
  Result := FStackPointer - FBasePointer;
end;

function TStack.NextLevel(Level: Integer): Integer;
begin
  Result := Level + 1;
  if Result > FMaxLevel then
  begin
    FMaxLevel := Result;
  end;
end;

procedure TStack.Pop(delta: Integer);
var
  x, sp: Integer;
  v:     PVariant;
begin
  sp := FStackPointer;
  //dirti hack.
  //todo: remove it
  {$push}
  {$r-}
  v  := @Data[sp];
  {$pop}
  sp := sp - delta;
  for x := 1 to delta do
  begin
    Dec(v);
    VarClear(v^);
  end;

  // Free memory
  FStackPointer := sp;

end;

// IncRecursion

procedure TStack.IncRecursion;
begin
  Inc(FRecursionDepth);
  if FRecursionDepth > FMaxRecursionDepth then
  begin
    raise EScriptError.CreateFmt(RTE_MaximalRecursionExceeded, [FMaxRecursionDepth]);
  end;
end;

// DecRecursion

procedure TStack.DecRecursion;
begin
  Dec(FRecursionDepth);
  Assert(FRecursionDepth>=0,'TStack: negative recursion depth');
end;


// GrowTo

procedure TStack.GrowTo(desiredSize: Integer);
begin
  if desiredSize > FMaxSize then
  begin
    raise EScriptError.CreateFmt(RTE_MaximalDatasizeExceeded, [FMaxSize]);
  end;
  FSize := ((desiredSize) div FChunkSize + 1) * FChunkSize;
  if FSize > FMaxSize then
  begin
    FSize := FMaxSize;
  end;
  SetLength(Data, FSize);
end;


// Push

procedure TStack.Push(Delta: Integer);
var
  sp: Integer;
begin
  sp := FStackPointer + Delta;

  // Increase stack size if necessary
  if sp > FSize then
  begin
    GrowTo(sp);
  end;

  FStackPointer := sp;
end;

procedure TStack.Reset;
var
  i: Integer;
begin
  Data  := nil;
  FSize := 0;
  FStackPointer := 0;
  FBasePointer := 0;
  ClearBpStore;
  SetLength(FBpStore, FMaxLevel + 1);
  for i := 0 to High(FBpStore) do
  begin
    FBpStore[i] := TSimpleStackInteger.Create;
    FBpStore[i].Push(0);
  end;
end;

procedure TStack.RestoreFrame(oldBasePointer: Integer);
begin
  FStackPointer := FBasePointer;
  FBasePointer  := oldBasePointer;
end;

// PushBp

procedure TStack.PushBp(Level, Bp: Integer);
begin
  Assert(Cardinal(Level) <= Cardinal(FMaxLevel));
  FBpStore[Level].Push(Bp);
end;

// GetSavedBp

function TStack.GetSavedBp(Level: Integer): Integer;
begin
  Assert(Cardinal(Level) <= Cardinal(FMaxLevel));
  Result := FBpStore[Level].Peek;
end;

// PopBp

function TStack.PopBp(Level: Integer): Integer;
begin
  Assert(Cardinal(Level) <= Cardinal(FMaxLevel));
  Result := FBpStore[Level].Pop;
end;

procedure TStack.SwitchFrame(var oldBasePointer: Integer);
begin
  oldBasePointer := FBasePointer;
  FBasePointer   := FStackPointer;
end;

procedure TStack.ReadData(SourceAddr, DestAddr, Size: Integer; DestData: TData);
begin
  while Size > 0 do
  begin
    VarCopy(DestData[DestAddr], Data[SourceAddr]);
    Inc(SourceAddr);
    Inc(DestAddr);
    Dec(Size);
  end;
end;

function TStack.ReadValue(SourceAddr: Integer): Variant;
begin
  Result := Data[SourceAddr];
end;

// ReadIntValue

function TStack.ReadIntValue(SourceAddr: Integer): Int64;
var
  varData: PVarData;
begin
  varData := @Data[SourceAddr];
  if varData^.VType = varInt64 then
  begin
    Result := varData^.VInt64;
  end else
  begin
    Result := PVariant(varData)^;
  end;
end;

// ReadIntAsFloatValue

procedure TStack.ReadIntAsFloatValue(SourceAddr: Integer; var Result: Double);
var
  varData: PVarData;
begin
  varData := @Data[SourceAddr];
  Assert(varData^.VType = varInt64);
  Result := varData^.VInt64;
end;

// ReadFloatValue

procedure TStack.ReadFloatValue(SourceAddr: Integer; var Result: Double);
var
  varData: PVarData;
begin
  varData := @Data[SourceAddr];
  if varData^.VType = varDouble then
  begin
    Result := varData^.VDouble;
  end else
  begin
    Result := PVariant(varData)^;
  end;
end;

// ReadStrValue

procedure TStack.ReadStrValue(SourceAddr: Integer; var Result: string);
var
  varData: PVarData;
begin
  varData := @Data[SourceAddr];
  if varData^.VType = varstring then
  begin
    Result := String(varData^.VString);
  end else
  begin
    Result := PVariant(varData)^;
  end;
end;

// ReadBoolValue

function TStack.ReadBoolValue(SourceAddr: Integer): Boolean;
var
  varData: PVarData;
begin
  varData := @Data[SourceAddr];
  if varData^.VType = varBoolean then
  begin
    Result := varData^.VBoolean;
  end else
  begin
    Result := PVariant(varData)^;
  end;
end;

// ReadInterfaceValue

procedure TStack.ReadInterfaceValue(SourceAddr: Integer; var Result: IUnknown);
var
  varData: PVarData;
begin
  varData := @Data[SourceAddr];
  if varData^.VType = varUnknown then
  begin
    Result := IUnknown(varData^.VUnknown);
  end else
  begin
    Result := PVariant(varData)^;
  end;
end;

// PointerToIntValue

function TStack.PointerToIntValue(addr: Integer): PInt64;
var
  varData: PVarData;
begin
  varData := @Data[addr];
  Assert(varData^.VType = varInt64);
  Result := @varData^.VInt64;
end;

// PointerToFloatValue

function TStack.PointerToFloatValue(addr: Integer): PDouble;
var
  varData: PVarData;
begin
  varData := @Data[addr];
  Assert(varData^.VType = varDouble);
  Result := @varData^.VDouble;
end;

// IncIntValue

procedure TStack.IncIntValue(destAddr: Integer; const Value: Int64);
var
  varData: PVarData;
begin
  varData := @Data[destAddr];
  Assert(varData^.VType = varInt64);
  varData^.VInt64 := varData^.VInt64 + Value;
end;

// AppendStringValue

procedure TStack.AppendStringValue(destAddr: Integer; const Value: string);

  procedure Fallback(varData: PVarData);
  begin
    PVariant(varData)^ := PVariant(varData)^ + Value;
  end;

var
  varData: PVarData;
begin
  varData := @Data[destAddr];
  if varData^.VType = varString then
  begin
    String(varData^.VString) := String(varData^.VString) + Value;
  end else
  begin
    Fallback(varData);
  end;
end;

// WriteData

procedure TStack.WriteData(SourceAddr, DestAddr, Size: Integer; const SourceData: TData);
begin
  while Size > 0 do
  begin
    Data[DestAddr] := SourceData[SourceAddr];
    Inc(SourceAddr);
    Inc(DestAddr);
    Dec(Size);
  end;
end;

// WriteValue

procedure TStack.WriteValue(DestAddr: Integer; const Value: Variant);
begin
  VarCopy(Data[DestAddr], Value);
end;

// WriteIntValue

procedure TStack.WriteIntValue(DestAddr: Integer; const Value: Int64);
var
  varData: PVarData;
begin
  varData := @Data[DestAddr];
  if varData^.VType = varInt64 then
  begin
    varData^.VInt64 := Value;
  end else
  begin
    PVariant(varData)^ := Value;
  end;
end;

// WriteIntValue

procedure TStack.WriteIntValue(DestAddr: Integer; const pValue: PInt64);
var
  varData: PVarData;
begin
  varData := @Data[DestAddr];
  if varData^.VType = varInt64 then
  begin
    varData^.VInt64 := pValue^;
  end else
  begin
    PVariant(varData)^ := pValue^;
  end;
end;

// WriteFloatValue

procedure TStack.WriteFloatValue(DestAddr: Integer; var Value: Double);
var
  varData: PVarData;
begin
  varData := @Data[DestAddr];
  if varData^.VType = varDouble then
  begin
    varData^.VDouble := Value;
  end else
  begin
    PVariant(varData)^ := Value;
  end;
end;

// WriteStrValue

procedure TStack.WriteStrValue(DestAddr: Integer; const Value: string);
var
  varData: PVarData;
begin
  varData := @Data[DestAddr];
  if varData^.VType = varString then
  begin
    String(varData^.VString) := Value;
  end else
  begin
    PVariant(varData)^ := Value;
  end;
end;

// WriteBoolValue

procedure TStack.WriteBoolValue(DestAddr: Integer; const Value: Boolean);
var
  varData: PVarData;
begin
  varData := @Data[DestAddr];
  if varData^.VType = varBoolean then
  begin
    varData^.VBoolean := Value;
  end else
  begin
    PVariant(varData)^ := Value;
  end;
end;

// WriteInterfaceValue

procedure TStack.WriteInterfaceValue(DestAddr: Integer; const intf: IUnknown);
var
  varData: PVarData;
begin
  varData := @Data[DestAddr];
  if varData^.VType = varUnknown then
  begin
    PUnknown(@varData^.VUnknown)^ := intf;
  end else
  begin
    PVariant(varData)^ := intf;
  end;
end;

// SetStrChar

function TStack.SetStrChar(DestAddr: Integer; index: Integer; c: Char): Boolean;
var
  varData: PVarData;
begin
  varData := @Data[DestAddr];
  if varData^.VType = varString then
  begin
    if index > Length(String(varData^.VString)) then
    begin
      Exit(False);
    end else
    begin
      String(varData^.VString)[index] := c;
    end;
  end else
  begin
    PVariant(varData)^[index] := c;
  end;
  Result := True;
end;

end.

