<?php

class A {
  public function foo(array $x) {
    return 1;
  }
}

class B extends A {
  //override
  public function foo($x) {
    return 2;
  }
}

$o = new B();
$o->foo(4);
