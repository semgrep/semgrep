<?php

// I have no idea what is the difference between using
// $o = new A(); and $o =& new A(); ...

// Drew: "Historically, '$a =& new A();' used to get special treatment
// because of how objects worked on PHP 4. HipHop currently has a bug
// where it converts '$a =& new A();' to '$a = new A();'

class A {
}

function test1() {
  $o = new A();
  var_dump($o);
  return $o;
}

$o = test1();
var_dump($o);

function &test2() {
  $o =& new A();
  var_dump($o);
  return $o;
}

$o2 = test2();
var_dump($o2);
