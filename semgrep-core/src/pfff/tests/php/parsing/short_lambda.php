<?php

function test_short_lambda() {
  $func = () ==> { return 1; };
  $func = async () ==> { return 1; };
}

function test_short_lambda_associativity() {
  $z = $x ==> $y ==> $x + $y;
}

function foo(): void {
  $func = $y ==> $y + 1;
  $func = async $y ==> $y + 1;
  $func = $y ==> $y + 2;
  $func = async $y ==> $y + 2;
  $func = $y ==> { return $y + 2; };
  $func = async $y ==> { return $y + 2; };
  $func = $y ==> { return $y - 2; };
  $func = async $y ==> { return $y - 2; };
  $func = async { return 0; };
  //$x = Vector { 1,2,3 };
  mapp($x, $func);
}

function foo2(): void {
  $y = ($a, $b) ==> $a + $b;
  $y = async ($a, $b) ==> $a + $b;
  $y = () ==> { return ($x, $y) ==> $x + $y; };
  $z = $y();
  if ($z(1, 2) < 10) { echo "yay\n"; }
  $y = async () ==> { return ($x, $y) ==> $x + $y; };
  $z = await $y();
  if ($z(1, 2) < 10) { echo "yay\n"; }
  $y = () ==> ($x, $y) ==> $x + $y;
  $y = async () ==> await async ($x, $y) ==> $x + $y;
  $z = await $y(1, 2);
  if ($z < 10) { echo "yay\n"; }
}
