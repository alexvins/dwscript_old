type TTest = class
   Field : String;
   procedure AppendString(str : String);
   class operator String uses AppendString;
end;

procedure TTest.AppendString(str : String);
begin
   Field:=Field+str+',';
end;

