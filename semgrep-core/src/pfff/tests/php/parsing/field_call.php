<?php

class A {
  public $fn;
  public function __construct() {
    $this->fn = function() { return 42; };
  }
}

$o = new A();
//$o->fn();
var_dump( ($o->fn)() );
var_dump(call_user_func($o->fn));
