<?php

// see http://us.php.net/const, a new PHP 5.3 feature

const Foo = 1;

const Bar = Foo;

$x = 1;

// this is ok
define('FooDefine', $x);
// this is not
//const FooBar = $x;
