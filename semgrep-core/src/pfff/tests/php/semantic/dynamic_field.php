<?php

class A {
  public $fld;
}

$o = new A();
$x = "fld";
$o->$x = 1;
$x = "fld2";
$o->$x = 1;
var_dump($o);
