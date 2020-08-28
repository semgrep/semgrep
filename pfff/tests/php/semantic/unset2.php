<?php

$a = 1;
$b = 2;

function foo(&$a) {
  global $a;
  $a++;
  unset($a);
  $a++;
}
foo($b);

echo $a, "\t", $b, "\n";
