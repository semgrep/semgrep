<?php

trait X {
  public function foo() {
    echo "X::foo()\n";
  }
}

class Y {
  use X
  {
    foo as old_foo;
    // if you write instead old_foo as foo
    // then you get a weird error message, ugly PHP
  }
  public function foobar() {
    $this->old_foo();
    // apparently this works too, ugly PHP
    $this->foo();
  }
}

$o = new Y();
$o->foobar();
