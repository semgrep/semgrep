<?php

function test_lambda() {
  $var = 1;
  $f = function($a) use(&$var) {
    $var += $a;
  };
  $f(3);
  var_dump($var);
}

// let's go
test_lambda();
