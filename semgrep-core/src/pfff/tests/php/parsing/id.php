<?php

class Cat {
  public function meow() {
    echo "miaou\n";
  }
}


// php 5.4 supports this:
//(new Cat())->meow();

// before we had to do this:
function id($o) { return $o;}
id(new Cat())->meow();
