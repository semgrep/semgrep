<?php

class A {
  public $fld = 42;
}
$o = new A();
var_dump($o);
$x = "A";
$o2 = new $x();
var_dump($o2);
