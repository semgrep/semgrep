<?php

class SFTHIS {
  static function not_dead() {
    echo "not_dead\n";
  }

  function bad_code() {

    // PHP allows this ... it should not
    $this->not_dead();
  }
}

$o = new SFTHIS();
$o->bad_code();
