<?php

// this file should satisfy the is_test_file() in database_light_php.ml
// which makes it possible to exercise the good_examples_of_use path
class C {
  static function b() {
    $o = new B();
  }
  static function c() {
    foobar();
  }
}

function test_foo() {
  foo();
}
