<?php

class StaticFunction {
  static function not_dead() {
    echo "not dead\n";
  }

  static function is_dead() {
    echo "dead\n";
  }
}


StaticFunction::not_dead();
