<?php

function foobar() {
}

function foo() {
  bar();
}

function bar() {
}

class A {
  static function mfoo() { foo(); }
  public function mbar() { bar(); }
}

class B extends A { }

interface I { }

trait T { }
