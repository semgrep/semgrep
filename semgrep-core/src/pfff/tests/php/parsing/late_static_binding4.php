<?php

// I don't understand late static binding, but I need to parse it at least ...
class A {
  public function getConstant() {
    return static::A_CONSTANT;
  }
}

// src: doug
// late static binding is like virtual functions, but for static class members,
// rather than instances of classes. the particular use case here is:
//
// php> class B { public static function foo() { return static::BAR; } }
// php> class X extends B { const BAR = "X"; }
// php> class Y extends B { const BAR = "Y"; }
// php> =X::foo()
// "X"
// php> =Y::foo()
// "Y"
