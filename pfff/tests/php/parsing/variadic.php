<?php

function foo($x, ...$rest) {
}

function bar($x, &...$rest) {
}

class Foo {
  public function foo(string $x, ...$xs): void {
  }

}
