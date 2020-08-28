<?php

function build_adder($x) {
  $f = (function ($y) use($x) {
      return $x + $y;
    });

  return $f;
}

$add1 = build_adder(1);
echo $add1(41);
