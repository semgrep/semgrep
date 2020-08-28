<?php

class Foo {
  static $foo = 42;
}

$test = 'Foo';
echo $test::$foo;
echo "\n";

$property = "foo";
echo $test::$$property;
echo "\n";
