<?php

class A {
}

class B {
  public function foo() {
    echo "B::foo\n";
  }
}

function foo() {
  $o = new A();
  $o = (new B())->foo();
}

foo();
