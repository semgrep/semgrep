<?php

// I don't understand late static binding, but I need to parse it at least ...
class A {
  public function getInstance() {
    if (static::$instance === null) {
      static::$instance = new static;
    }
    return static::$instance;
  }
}
