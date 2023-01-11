<?php

class A {
  protected $fld = 42;
  public function test() {
    var_dump($this->fld);
  }
}

class B extends A {
  protected $fld = 2;
}

$a = new A();
$b = new B();
$a->test();
$b->test();
