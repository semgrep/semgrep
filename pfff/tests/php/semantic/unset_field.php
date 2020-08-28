<?php

class A {
  public $fld;
}

$o = new A();
var_dump($o);
unset($o->fld);
var_dump($o);
