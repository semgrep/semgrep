<?php

function __builtin__echo() { }

class A {
  private static $instance = null;

  public static function getInstance() {
    if (!self::$instance) {
      self::$instance = new A();
    }
    return self::$instance;
  }

  public function foo() {
    echo "A::foo\n";
  }
}

class B {
  public function foo() {
  }
}

function main() {
  // the interpreter should correctly go to A::foo
  A::getInstance()->foo();
}

main();
