<?php

// http://php.net/manual/en/language.oop5.traits.php
// latest spec: http://docs.php.net/traits

trait Company {
  public function getName() {
    return 'Facebook';
  }
}

class Language { }

class English extends Language {
  use Company;
  public function sayHello() {
    echo 'Hello' . $this->getName();
  }
}

class Portuguese extends Language {
  use Company;
  public function sayHello() {
    echo 'Oi ' . $this->getName();
  }
}

$o = new English();
$o->sayHello();
