
var i : Integer = 0;
var j : Integer = 1;

procedure PrintIJ;
begin
   PrintLn(IntToStr(i)+' / '+IntToStr(j));
end;

PrintIJ;

i:=i;
j:=j;

PrintIJ;

i:=i-1;
j:=j+1;

PrintIJ;

i:=i-j;
j:=j+i;

PrintIJ;

i:=-i;
j:=j+j;

PrintIJ;
