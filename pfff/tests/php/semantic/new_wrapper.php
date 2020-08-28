<?php

function newv($string) {
  return new $string();
}

class A {
  public $fld = 1;
}

$o = newv('A');
var_dump($o);
