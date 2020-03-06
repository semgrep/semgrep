<?php

// sgrep -e 'array(1 => "foo")' *.php

class A {
  //TODO: the pattern above currently does not match this :(
  // because in the AST they are represented in different ways.
  // maybe julien is right ...
  static $x = array(1 => 'foo');
}

$x = array(1 => 'foo');
