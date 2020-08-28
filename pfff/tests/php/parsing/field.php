<?php

class X {
  public $afield = 1;
  public static $astatic = 2;
  const aconstant = 3;

}

$o = new X();
var_dump($o->afield);

// forbidden
//var_dump($o->astatic);
//var_dump($o->aconstant);

var_dump(X::$astatic);
var_dump(X::aconstant);
