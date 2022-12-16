<?php

class A {
  function foo() { }
}
class B extends A {
  function foo() {
    self::foo();
    parent::foo();
  }
}
