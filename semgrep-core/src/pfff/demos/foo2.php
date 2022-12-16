<?php

require_once $_SERVER['PHP_ROOT'].'/lib/foo.php';
require_module('foo/bar');

class Foo extends Bar {
  public function foo() {
    foo_func();
  }

  public static function bar() {
    $bar = new Bar();
    return $bar->bar();
  }
}

function bar_func() {
  return 1;
}
