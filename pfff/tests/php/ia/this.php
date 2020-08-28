<?php

class A {
  private $fld;
  public function __construct($x) {
    $this->fld = $x;
  }
  public function foo() {
    var_dump($this->fld);
  }
  public function bar() {
    $this->foo();
  }
}

$o = new A(42);
$o2 = new A(55);
$o->bar();
$o2->bar();
