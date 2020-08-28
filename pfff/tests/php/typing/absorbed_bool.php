<?php

//CONFIG: TODO this can be changed so that foo:: int|bool -> int|bool
// instead of foo:: int->int

function foo($x) {
  $x = 1;
  $x = true;
  // int > bool
  return $x;
}
