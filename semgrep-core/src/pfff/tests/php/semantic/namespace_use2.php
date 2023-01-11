<?php

require 'namespace.php';

use X\A;
$o = new A();

use X\A as B;
$o = new B();

X\foo();
// does not work for functions
use X\foo;
//foo(); // does not work

// does not work for constants
$v = X\CST;
var_dump($v);
