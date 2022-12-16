<?php

function foo($x) {
  $x = 1;
  $y = $x;
  // note that type inference is flow insensitive, so
  // growing the type of $x (union(int, array)) will grow the
  // type of $y too.
  $x = array(1);
  return $y;
}
