<?php

//PHP keywords can be used as fields

class A {
  public $if;
  public $parent;
  function __construct() {
    $this->if = 1;
    $this->parent = 2;
  }
}

$o = new A();
echo $o->if;
echo $o->parent;

$a = Entity::for;
$b = Entity::GLOBAL;
$c = Entity::AS;
