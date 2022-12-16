<?php

class A {
  public function foo() {
    echo "A::foo()\n";
  }
}

class B extends A {
  public function __call($f, $args) {
    echo "B::__call($f, $args)\n";
  }
}

$o = new B();
// this will call A::foo. The __call is used only when
// nothing was found in the whole hierarchy, not just
// when nothing was found in your class.
$o->foo();
// this will call B::__call
$o->bar();
