<?php

class A {
  public function foo() { }
}

class B {
  public function foo() { }
}


function main() {
  $x = null;
  if(true) {
    $x = new A();
  } else {
    $x = new B();
  }
  //TODO: it should call both methods ...
  $x->foo();
  //var_dump($x);
}

main();
