<?php

class DefaultPublic {
  static $foo = 1;
  static private $bar = 1;
  public function __construct() {
  }
}

var_dump(DefaultPublic::$foo);
//var_dump(DefaultPublic::$bar);
