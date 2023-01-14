<?php

// use --pretty-printer

function test1() {

  return foo('a                                                            ');
}

// we should keep unrelated code unchanged, even if badly indented
function not_related() {
  bar(
    1
  );
}

function test2() {
  return foo(1);
}

interface InterfaceFoo {
  public function test1();
}

class Foo {
  public function test1() {
    return foo('a                                                         ');
  }

  public function not_related() {
    bar(
      1
    );
  }
}

