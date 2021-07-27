int x = 5;
var reference = __makeref(x);
Console.WriteLine(__refvalue(reference, int));
Console.WriteLine(__reftype(reference));

__refvalue(reference, int) = 10;
Console.WriteLine(__refvalue(reference, int));
