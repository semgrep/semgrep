<?php
class Foo {
  //ERROR: match
  public function __construct($path) {
    file_get_contents($path);
  }
}
