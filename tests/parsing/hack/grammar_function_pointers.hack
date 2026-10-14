<?hh
$f = foo<>;
$typed = fizz<int, string>;
$method = MyClass::bar<>;
$namespaced = \Foo\Bar\baz<>;
foo<int, string>();
MyClass::bar<int>();
