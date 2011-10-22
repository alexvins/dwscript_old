// array params are not supported because of dwsComConnector.DispatchInvoke limitations
// if you want to change that, feel free to submit source code upgrades!

var DispVar : ComVariant := DispCallProxy;

Procedure Proc(Const AParams : Array Of Const);
Begin
 PrintLn(DispVar.methodCall(AParams));
End;

Proc(['1', '2', '3']); 