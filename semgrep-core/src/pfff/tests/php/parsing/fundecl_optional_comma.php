<?php

function test_optional_comma_fundecl(
  $x,
  $y,
  $z,
) {
}


function test_optional_comma_closure($param) {
  $closure = function($a) use ($param,) { return $a + $param; };
  return $closure;
}
