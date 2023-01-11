<?php

function taking_a_ref(&$x) {
  $x = $x + 10;
}

$x = 0;
taking_a_ref($x = 2);
// output = 2 ... insane
echo $x;

$x = 0;
taking_a_ref($x);
echo $x;

function taking_an_array_ref(&$x) {
  $x[] = 'foo';
}

taking_an_array_ref($array = array());
// output is empty array, ... insane
var_dump($array);

$array = array();
taking_an_array_ref($array);
var_dump($array);
