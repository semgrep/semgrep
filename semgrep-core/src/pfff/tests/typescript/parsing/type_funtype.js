
var id = function<T>(x:T):T { return x; }
var o: { f<T>(x:T):T; } = { f: id }
