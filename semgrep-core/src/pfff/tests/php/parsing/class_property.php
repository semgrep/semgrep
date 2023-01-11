<?php

// Java calls that class property, PHP class name resolution
// https://wiki.php.net/rfc/class_name_scalars

class C {
}
interface I {
}
trait T {
}
abstract class A {
  public static function testA() {
    echo "A::testA\n";
    var_dump(static::class);
  }
}

class B extends A {
  public function test() {
    echo "B::testA\n";
    var_dump(self::class);
    var_dump(parent::class);
  }
}

function test() {
    echo "test\n";
  var_dump(C::class);
  var_dump(I::class);
  var_dump(T::class);
  var_dump(A::class);

  $o = new C();
  var_dump($o::class);
}

test();

$b = new B();
$b->test();
$b::testA();
