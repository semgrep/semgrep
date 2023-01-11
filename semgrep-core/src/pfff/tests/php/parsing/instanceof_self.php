<?php

class A {

  static function foo() {
    $o = new A();
    if($o instanceof self) {
      echo "HERE\n";
    }
  }
}

A::foo();
