<?php

class A {
  protected function foo() {
    echo "A::protected::foo";
  }
}

$o = new A();
//HipHop Fatal error: Attempt to call protected A::foo()
//Good, PHP is retarded but not that retarded
$o->foo();
