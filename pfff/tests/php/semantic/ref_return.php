<?php

// Functions can return references (although people usually use reference in
// arguments as in foo($x, $result) where 'function foo($x, &$result) {... }').
// Note that you need both the ref annotation here and at the call site
// (which is inconsistent with how you use reference elsewhere, but hey,
// it's PHP, so inconsistency is the norm).
// It's the only place where you can say something about the return value
// (you have type hints for arguments but not for the return value)
function &foo() {
  static $x = 1;
  $x = $x+1;
  return $x;
}
// this is required too, without it there is a copy.
$y =& foo();

var_dump($y);
$y = 0;
$z = foo();
var_dump($z);
