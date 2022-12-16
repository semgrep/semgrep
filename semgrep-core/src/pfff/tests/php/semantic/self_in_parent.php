<?php

class A {
  static function foo() {
    self::bar();
  }

  static function bar() {
    echo "A::bar()";
  }
}

class B extends A {
  static function bar() {
    echo "B::bar()";
  }
}

// will print A::bar(). Use static: if you want the (retarded) other
// behavior
B::foo();
