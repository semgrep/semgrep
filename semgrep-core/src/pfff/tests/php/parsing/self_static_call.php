<?php

class CallSelf {
  public static function foo() {
    echo "foo\n";
  }
  public static function test() {
    self::foo();
  }
}

CallSelf::test();
