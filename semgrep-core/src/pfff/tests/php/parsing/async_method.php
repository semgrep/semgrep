<?php

class Foo {
  // this should be forbidden now that async is a keyword, but
  // some of our legacy code do that
  public function async() {
  }
}

$o = new Foo();
$o->async();
