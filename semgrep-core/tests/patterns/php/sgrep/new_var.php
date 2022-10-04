<?php

$o = new Foo;
$o = new Foo();

class Bar {
  public function test() {
    $o = new self();
  }
}

$o = new $dyn(1);
