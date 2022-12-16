<?php

//http://www.php.net/manual/en/language.namespaces.rationale.php
namespace my\name;

class Cat {
  static function says() {
    echo 'meoow\n';
  }
}

$o = new Cat();
$o->says();

$o = new \my\name\Cat();
$o->says();

echo __NAMESPACE__;
