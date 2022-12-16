<?php

class A {
  static function mkobj() {
    return new self();
  }
}

var_dump(A::mkobj());
