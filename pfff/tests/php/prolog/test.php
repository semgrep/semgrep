<?php

//procedure:
// $ ~/pfff/pfff_db_heavy -metapath /tmp/test_db/ .
// $ ~/pfff/pfff_db_heavy -gen_prolog_db /tmp/test_db/ /tmp/facts.pl
// $ swipl -s /tmp/facts.pl -f prolog_code.pl -t halt --quiet -g "..."

function foo($o) {
  $o['name'] = "foo";
  bar();
}

function bar($o) {
  echo $o['name'];
}

class A {
  public $fld;

  public function methA() {
    foo();
  }
}

class B extends A {
  public function methA() {
    bar();
    echo $this->fld;
  }
  public function methB() {
    $this->fld = 1;
  }
}

interface I {
  public function inter();
}

class C extends B implements I {
  public function inter() {
    bar();
  }
}

function test_long_field($o) {
  echo $o["this is a'valid field too"];
}

interface J {
}

interface K extends J {
}

class D extends K {
}

trait T {
  //use X;
  public function trait1() {
    echo "trait1\n";
  }
}

class E {
  use T;
}
