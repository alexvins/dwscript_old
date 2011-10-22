unit UdwsUtilsTests;

interface

uses Windows, Classes, SysUtils, TestFrameWork, dwsUtils, dwsJSON;

type

   TdwsUtilsTests = class (TTestCase)
      private
         FTightList : TTightList;

      protected
         procedure TightListOutOfBoundsDelete;
         procedure TightListOutOfBoundsInsert;
         procedure TightListOutOfBoundsMove;

      published

         procedure StackIntegerTest;
         procedure WriteOnlyBlockStreamTest;
         procedure WOBSBigFirstTest;
         procedure TightListTest;
         procedure LookupTest;

         procedure JSONTest;
         procedure ParseJSON;

         procedure UnicodeCompareTextTest;

         procedure VarRecArrayTest;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsUtilsTests ------------------
// ------------------

// StackIntegerTest
//
procedure TdwsUtilsTests.StackIntegerTest;
var
   stack : TSimpleStack<Integer>;
begin
   stack:=TSimpleStack<Integer>.Create;

   CheckEquals(0, stack.Count);

   stack.Push(123);

   CheckEquals(1, stack.Count);
   CheckEquals(123, stack.Peek);

   stack.Push(456);

   CheckEquals(2, stack.Count);
   CheckEquals(456, stack.Peek);

   CheckEquals(456, stack.Peek);
   stack.Pop;

   CheckEquals(1, stack.Count);

   CheckEquals(123, stack.Peek);
   stack.Pop;

   CheckEquals(0, stack.Count);

   stack.Free;
end;

// WriteOnlyBufferBlockTest
//
procedure TdwsUtilsTests.WriteOnlyBlockStreamTest;
var
   buffer : TWriteOnlyBlockStream;
   b : Integer;
   bs : AnsiString;
begin
   buffer:=TWriteOnlyBlockStream.Create;

   CheckEquals(0, buffer.Position);

   b:=Ord('1');
   buffer.Write(b, 1);
   CheckEquals(1, buffer.Position);

   bs:='23';
   buffer.Write(bs[1], 2);
   CheckEquals(3, buffer.Position);

   for b:=Ord('4') to Ord('9') do
      buffer.Write(b, 1);
   CheckEquals(9, buffer.Position);

   SetLength(bs, buffer.Size);
   buffer.StoreData(bs[1]);

   buffer.Free;

   CheckEquals(AnsiString('123456789'), bs);
end;

// WOBSBigFirstTest
//
procedure TdwsUtilsTests.WOBSBigFirstTest;
var
   buffer : TWriteOnlyBlockStream;
   i : Integer;
   bw, br : TBytes;
begin
   buffer:=TWriteOnlyBlockStream.Create;

   SetLength(bw, cWriteOnlyBlockStreamBlockSize*2);
   for i:=0 to High(bw) do
      bw[i]:=Byte(i and 255);

   buffer.Write(bw[0], Length(bw));

   CheckEquals(Length(bw), buffer.Size, 'size');

   SetLength(br, buffer.Size);
   buffer.StoreData(br[0]);

   for i:=0 to High(br) do
      if br[i]<>bw[i] then
         CheckEquals(bw[i], br[i], IntToStr(i));

   buffer.Free;
end;

// TightListOutOfBoundsDelete
//
procedure TdwsUtilsTests.TightListOutOfBoundsDelete;
begin
   FTightList.Delete(-1);
end;

// TightListOutOfBoundsInsert
//
procedure TdwsUtilsTests.TightListOutOfBoundsInsert;
begin
   FTightList.Insert(999, nil);
end;

// TightListOutOfBoundsMove
//
procedure TdwsUtilsTests.TightListOutOfBoundsMove;
begin
   FTightList.Insert(1, nil);
end;

// TightListTest
//
procedure TdwsUtilsTests.TightListTest;
begin
   CheckEquals(-1, FTightList.IndexOf(nil), 'empty search');

   CheckException(TightListOutOfBoundsDelete, ETightListOutOfBound, 'OutOfBounds Delete');
   CheckException(TightListOutOfBoundsInsert, ETightListOutOfBound, 'OutOfBounds Insert');
   CheckException(TightListOutOfBoundsMove, ETightListOutOfBound, 'OutOfBounds Move');

   FTightList.Add(Self);
   CheckEquals(-1, FTightList.IndexOf(nil), 'single search nil');
   CheckEquals(0, FTightList.IndexOf(Self), 'single search Self');

   FTightList.Move(0, 0);

   CheckEquals(0, FTightList.IndexOf(Self), 'single search Self 2');

   FTightList.Add(nil);
   CheckEquals(1, FTightList.IndexOf(nil), 'two search nil');
   CheckEquals(0, FTightList.IndexOf(Self), 'two search Self');
   CheckEquals(-1, FTightList.IndexOf(Pointer(-1)), 'two search -1');

   FTightList.Move(0, 1);

   CheckEquals(0, FTightList.IndexOf(nil), 'two search nil 2');
   CheckEquals(1, FTightList.IndexOf(Self), 'two search Self 2');

   FTightList.Move(1, 0);

   CheckEquals(1, FTightList.IndexOf(nil), 'two search nil 3');
   CheckEquals(0, FTightList.IndexOf(Self), 'two search Self 3');

   FTightList.Add(nil);
   FTightList.Move(2, 0);

   CheckEquals(0, FTightList.IndexOf(nil), 'three search nil');
   CheckEquals(1, FTightList.IndexOf(Self), 'three search Self');

   FTightList.Clear
end;

// LookupTest
//
procedure TdwsUtilsTests.LookupTest;
var
   lookup : TObjectsLookup;
   obj : TObject;
begin
   lookup:=TObjectsLookup.Create;
   try
      CheckFalse(lookup.IndexOf(nil)>=0, 'empty');
      lookup.Add(nil);
      CheckTrue(lookup.IndexOf(nil)>=0, 'nil');
      obj:=TObject.Create;
      CheckFalse(lookup.IndexOf(obj)>=0, 'obj');
      lookup.Add(obj);
      CheckTrue(lookup.IndexOf(nil)>=0, 'nil bis');
      CheckTrue(lookup.IndexOf(obj)>=0, 'obj bis');
      lookup.Clean;
   finally
      lookup.Free;
   end;
end;

// JSONTest
//
procedure TdwsUtilsTests.JSONTest;
var
   json : TdwsJSONObject;
begin
   json:=TdwsJSONObject.Create;

   CheckEquals('{}', json.ToString);
   CheckEquals('{ }', json.ToBeautifiedString(0, 3));

   json.AddValue('hello').AsString:='world';

   CheckEquals('{"hello":"world"}', json.ToString);
   CheckEquals('{'#13#10#9'"hello" : "world"'#13#10'}', json.ToBeautifiedString(0, 1));

   with json.AddArray('items') do begin
      AddValue;
      AddValue.AsNumber:=12.3;
      AddValue.AsBoolean:=True;
      AddValue.AsBoolean:=False;
      AddValue.IsNull:=True;
   end;

   CheckEquals('{"hello":"world","items":[null,12.3,true,false,null]}', json.ToString);
   CheckEquals( '{'#13#10
                  +#9'"hello" : "world",'#13#10
                  +#9'"items" : ['#13#10
                     +#9#9'null,'#13#10
                     +#9#9'12.3,'#13#10
                     +#9#9'true,'#13#10
                     +#9#9'false,'#13#10
                     +#9#9'null'#13#10
                  +#9']'#13#10
               +'}', json.ToBeautifiedString(0, 1));

   json.Free;
end;

// ParseJSON
//
procedure TdwsUtilsTests.ParseJSON;
var
   json : TdwsJSONValue;
   sl : TStringList;
begin
   json:=TdwsJSONValue.ParseString('"hello"');
   CheckEquals(TdwsJSONImmediate.ClassName, json.ClassName, '"hello"');
   CheckEquals('"hello"', json.ToString, '"hello"');
   json.Free;

   json:=TdwsJSONValue.ParseString('{"hello":"world","abc":123}');
   CheckEquals(TdwsJSONObject.ClassName, json.ClassName, '"hello"');
   CheckEquals('{"hello":"world","abc":123}', json.ToString, '"hello"');
   json.Free;

   sl:=TStringList.Create;
   try
      sl.LoadFromFile(ExtractFilePath(ParamStr(0))+'\Data\json.txt');
      json:=TdwsJSONValue.ParseString(sl.Text);
      CheckEquals(TdwsJSONObject.ClassName, json.ClassName, 'json.txt');
      CheckEquals(1, json.ElementCount, 'json.txt');
      CheckEquals(3, json.Elements[0].ElementCount, 'json.txt');
      CheckEquals('"templates"', json[0]['servlet'][0]['init-param']['templatePath'].ToString, 'json.txt');
      CheckEquals('', json['doh'][5]['bug'].ToString, 'json.txt');
      json.Free;
   finally
      sl.Free;
   end;
end;

// UnicodeCompareTextTest
//
procedure TdwsUtilsTests.UnicodeCompareTextTest;
begin
   CheckTrue(UnicodeCompareText('', '')=0, 'both empty');

   CheckTrue(UnicodeCompareText('a', '')>0, 'a, empty');
   CheckTrue(UnicodeCompareText('', 'a')<0, 'empty, a');
   CheckTrue(UnicodeCompareText('�', '')>0, '�, empty');
   CheckTrue(UnicodeCompareText('', '�')<0, 'empty, �');

   CheckTrue(UnicodeCompareText('abc', 'abc')=0, 'abc, abc');
   CheckTrue(UnicodeCompareText('abcd', 'abc')>0, 'abcd, abc');
   CheckTrue(UnicodeCompareText('abc', 'abcd')<0, 'abc, abcd');
   CheckTrue(UnicodeCompareText('abc', 'abd')<0, 'abc, abd');
   CheckTrue(UnicodeCompareText('abd', 'abc')>0, 'abc, abd');

   CheckTrue(UnicodeCompareText('abe', 'ab�')<0, 'abe, ab�');
   CheckTrue(UnicodeCompareText('ab�', 'abe')>0, 'ab�, abe');
   CheckTrue(UnicodeCompareText('ab�a', 'ab�z')<0, 'ab�a, ab�z');
   CheckTrue(UnicodeCompareText('ab�z', 'ab�a')>0, 'ab�z, ab�a');

   CheckTrue(UnicodeCompareText('ab�', 'ab�')=0, 'ab�, ab�');
   CheckTrue(UnicodeCompareText('ab�aa', 'ab�z')<0, 'ab�aa, ab�z');
   CheckTrue(UnicodeCompareText('ab�z', 'ab�aa')>0, 'ab�z, ab�aa');
end;

// VarRecArrayTest
//
procedure TdwsUtilsTests.VarRecArrayTest;
var
   v : TVarRecArrayContainer;
begin
   v:=TVarRecArrayContainer.Create;
   try
      v.Add(True);
      v.Add(False);

      CheckEquals(2, Length(v.VarRecArray));

      CheckEquals(vtBoolean, v.VarRecArray[0].VType, 'type 0');
      CheckEquals(True, v.VarRecArray[0].VBoolean, 'value 0');

      CheckEquals(vtBoolean, v.VarRecArray[1].VType, 'type 1');
      CheckEquals(False, v.VarRecArray[1].VBoolean, 'value 1');
   finally
      v.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TestFramework.RegisterTest('dwsUtilsTests', TdwsUtilsTests.Suite);

end.
