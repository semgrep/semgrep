<?php

class A {
//  public function foo() {
//    return array('x1' => new A(),
//                 'x2' => new A());
//  }
//  public function bar() {
//    echo "A->bar\n";
//  }
}

//$o = new A();
$o->foo()['x1']->bar();
