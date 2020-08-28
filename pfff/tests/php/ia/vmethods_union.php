<?php

class A {
  private $x = array();

  public function m() {
    return $this->x;
  }
}

class B {
  private $x = false;

  public function m() {
    return $this->x;
  }
}

if(true) {
  $x = new A();
}
else {
  $x = new B();
}

var_dump($x);

//TODO we are not path sensitive, so it should call both classes
$x->m();
