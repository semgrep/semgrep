<?php

class A {
  public $_x;
  function __construct($x) {
    $this->_x = $x;
  }

  function foo() {
    echo "A::foo, _x = " . $this->_x;
  }
}

class B extends A {

  public function foo() {
    echo "B::boo\n";
    parent::foo();
  }
}

$o = new B(3);
$o->foo();
