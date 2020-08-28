<?php

class SF3 {
  static function not_dead() {
    echo "not dead\n";
  }


  static function call_not_dead() {
    //SF3::not_dead();
    self::not_dead();
  }

  static function is_dead() {
    echo "dead\n";
  }
}


SF3::call_not_dead();
