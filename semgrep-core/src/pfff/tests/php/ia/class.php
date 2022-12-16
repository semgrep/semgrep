<?php

class A { public function foo() { } }
$x = new A();
//TODO: why objects are represented as a pointer to a shared ref to a value?
var_dump($x);
