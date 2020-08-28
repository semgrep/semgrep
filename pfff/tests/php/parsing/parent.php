<?php

// parent/self are kinda keywords in PHP, but PHP allow to name
// methods or functions with those keywords ...

class A {
  function parent() {
    return 1;
  }
  function self() {
    return 1;
  }
}

$o = new A();
echo $o->parent();
