var i : Integer;
for i:=1 to 10 do
   if i not in [2, 4, 6..8] then
      PrintLn(i);

PrintLn(BoolToStr(1 not in []));

if 'A' not in ['A'..'Z'] then
   PrintLn('A not in A..Z');

if 'a' not in ['A'..'Z'] then
   PrintLn('a not in A..Z');
