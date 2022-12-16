<?php

class A {
  public function __construct() {
  }
  public function foo() {
    echo "A::foo\n";
  }
}

class B extends A {
  public function __construct() {
  }
  public function foo() {
    echo "B::foo\n";
  }
}


$o = null;
$cond = true;
if($cond) {
  $o = new A();
} else {
  $o = new B();
}

// When in tracing mode it should say A::foo or B::foo
// TODO: right now julien picks one in the Vsum :(
$o->foo();
