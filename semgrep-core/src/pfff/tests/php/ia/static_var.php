<?php

function foo() {
  static $x = 1;
  var_dump($x);
  $x = 2;
}

foo();
foo();
