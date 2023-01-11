<?php

function __builtin__echo() {
}

trait T {
  public function foo() {
    echo "T::foo()\n";
    return 1;
  }
}

class A {
  public function foo() {
    echo "A::foo()\n";
    return true;
  }
}

class B extends A {
  use T;
}

class C extends B {
}

$o = new C();
// this will call T::foo(), traits are look first, before inheritance
$o->foo();
