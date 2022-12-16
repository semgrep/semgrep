<?php

class A {
  const XXX = 2;
  static $x;
}

A::$x = 1;
echo A::$x;

$x = 'XXX';
echo A::$x;
