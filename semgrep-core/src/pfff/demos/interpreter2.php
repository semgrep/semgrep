<?php

function foo() {

  $t = array(1,2);
  $x = 1;
  $y = 42;
  $z = &$x;

  if($x === 1) {
    $y = 42;
  } else {
    $y = 43;
  }
  //show($y);
  ;
}

foo();
