if Chr($41)<>'A' then PrintLn('bug A');
if Chr($263A)<>'☺' then PrintLn('bug smiling face');
if Chr($10000)<>#$D800#$DC00 then PrintLn('bug U+10000');
if Chr($103FF)<>#$D800#$DFFF then PrintLn('bug U+103FF');
if Chr($10FC00)<>#$DBFF#$DC00 then PrintLn('bug U+10FC00');
if Chr($10FC01)<>#$DBFF#$DC01 then PrintLn('bug U+10FC01');
if Chr($10FFFF)<>#$DBFF#$DFFF then PrintLn('bug U+10FFFF');
PrintLn(Chr($42));
