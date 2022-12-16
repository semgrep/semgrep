<?php

class A { }
$o = new A();

//TODO: throw an UnknownConstant("A")
if($o instanceof A) {
  var_dump($o);
}
