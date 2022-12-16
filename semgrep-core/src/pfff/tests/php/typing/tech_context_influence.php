<?php

function myid($x) {
  return $x;
}
function context_influence() {
  $x = myid(1);
  return $x;
}
function context_influence2() {
  $x = myid(true);
  return $x;
}

class Foo {
  static function myid($x) {
    return $x;
  }
}
function foo_context_influence() {
  $x = Foo::myid(1);
  return $x;
}

function foo_context_influence2() {
  $x = Foo::myid(true);
  return $x;
}

class Foo2 {
  static function myid($x) {
    return $x;
  }
  static function context_influence() {
    $x = Foo::myid(1);
    return $x;
  }
  static function context_influence2() {
    $x = Foo::myid(true);
    return $x;
  }
}
