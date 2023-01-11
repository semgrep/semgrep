<?php

class A {
  public function __construct() {
  }

  public function foo() {
  }
}

$o = new A();
$o->foo();
//$o->bar();
$x = null;
$x->foo();
