<?hh
// ERROR: match
$x = new MyClass<int>(1);
$y = new MyClass<string>(1);
$z = new MyClass(1);
$w = new Other<int>(1);
