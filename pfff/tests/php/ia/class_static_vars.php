<?php

class A {
  public $fld = 1;
  static public $fld2 = 2;
  public function foo() { }
  static public function bar() { }
  const CST = 2;
}

$o = new A();
$o2 = new A();
// both objects should share the same pointer for the $fld2 static variable
var_dump($o);
var_dump($o2);

var_dump($o::$fld2);
//this is incorrect
//var_dump($o->$fld2);

//this is will return null
var_dump($o->fld2);
