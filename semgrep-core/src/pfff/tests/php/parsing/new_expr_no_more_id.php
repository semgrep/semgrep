<?php

class NoId {
  public function foo() {
    echo "foo\n";
  }
  public function bar() {
    echo "bar\n";
  }
}

function id($x) {
  return $x;
}

$o = (new NoID)->foo()->bar();
$o = (new NoID())->foo()->bar();

// old way
$o = id(new NoID)->foo()->bar();

// TODO?
//function test_new_expr() {
//  (new NoId())->meh = 10;
//}

//function test_new_expr2() {
//  ((new NoId()))->meh();
//}
