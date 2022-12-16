<?php

class A1 {
  public static function getInstance() {
    return new self();
  }
}

class B1 extends A1 {
}

$obj = B1::getInstance();
var_dump($obj);
//object(A1)#1 (0) { }


class A {
  public static function getInstance() {
    return new static();
  }
}

class B extends A {
}

$obj = B::getInstance();
var_dump($obj);
//object(B)#2 (0) { }
