<?php
// https://wiki.php.net/rfc/shortsyntaxforarrays

$a = [1, 2, 3];
$b = array(1, 2, 3);
var_dump($a);
var_dump($b);

function f() {
  return 42;
}

$x = [f()];
var_dump($x);
