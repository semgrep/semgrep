<?php

function foo($x) {
  $x = $x + 10;
  return $x;
}

$y = 5;
foo($y);
//will print 5
var_dump($y);


$y = 5;
// ugly, really ugly, $y will be modified even if
// 'foo' does not say it's using a reference.
// CHECK: we should forbid passing a ref to a function not expecting one
foo(&$y);
var_dump($y);
