<?php

class A {
  public static function foo($x) {
    var_dump($x);
  }
}

$x = 'A::foo';
// this is not ok
//$x(1);
//TODO this is not handled right now
call_user_func($x, 42);
