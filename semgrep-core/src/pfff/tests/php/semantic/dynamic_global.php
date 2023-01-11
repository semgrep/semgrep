<?php

$g1 = 42;

function test() {
  $o = "g1";
  // without this line it would print "undefined variable g1"
  global $$o;
  echo $$o;
}

test();
