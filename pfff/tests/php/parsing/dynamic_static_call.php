<?php

class A {
  static function foo() { echo "foo\n"; }
}

$s = 'A';
$s::foo();
