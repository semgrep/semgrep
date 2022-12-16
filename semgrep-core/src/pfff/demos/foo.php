<?php

function foo($a) {
  echo $a;
}

function foo2($a) {
  foo($a);
}

foo("hello world");
foo2("hello world");
