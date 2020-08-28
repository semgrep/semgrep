<?php

function test_shadowing() {

  $x = 42;
  $array = array(1,2,3);
  foreach ($array as $x) {
    echo "$x\n";
  }
  echo $x;
}

test_shadowing();
