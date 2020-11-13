// https://stackoverflow.com/questions/7452341/what-does-void-0-mean/7452352#7452352

// ok
alert(undefined); //alerts "undefined"
// ruleid:assigned-undefined
var undefined = "new value";
alert(undefined) // alerts "new value"

// ruleid:assigned-undefined
undefined = "new value";
alert(undefined) // alerts "new value"
