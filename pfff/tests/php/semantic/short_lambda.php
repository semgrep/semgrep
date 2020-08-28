<?php

function test_short_lambda() {
  $a = 42;
  $f = () ==> $a;
  echo $f();
}

function test_long_lambda() {
  $a = 42;
  // this does not close
  $f = (function () { return $a; });
  echo $f();
}


test_short_lambda();
test_long_lambda();
