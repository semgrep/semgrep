<?php

use Foo\Bar\{ClassA, ClassB, ClassC as C};

$obja = new ClassA();
$obja = new ClassB();
// ERROR: match
$obja = new C();
$obja = new ClassC();
