<?php

class A {
  public $fld;
}
class B {
}

$o = new A();
$o->fld = "b";

$o2 = new $o->fld;
var_dump($o2);
