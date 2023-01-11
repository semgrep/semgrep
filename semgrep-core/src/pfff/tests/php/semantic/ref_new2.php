<?php

function f() {
  $y = 123;
  $x =& $y;
  $x =& new stdclass;
  // Zend outputs false, HPHP outputs true
  var_dump(is_object($y));
  var_dump($y);
}

f();
