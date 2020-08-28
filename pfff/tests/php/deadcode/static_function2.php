<?php

class StaticFunctionBase2 {
  static function not_dead() {
    echo "not dead\n";
  }

  static function is_dead() {
    echo "dead\n";
  }
}

class StaticFunction2 extends StaticFunctionBase2 {
}

StaticFunction2::not_dead();
