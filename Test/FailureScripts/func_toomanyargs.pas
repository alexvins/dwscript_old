procedure MyProc(a : Integer);
begin
end;

IntToStr(45, 12);
MyProc(45, 12);

var v := '12';
MyProc(45, StrToInt(v));
MyProc(45, StrToInt(v, 1, 2));
