<?php

class A {
}

class B {
}

$o = new A();

if($o instanceof A) {
  echo "o is of class A\n";
}

if($o instanceof B) {
  echo "o is of class B\n";
}

if($o instanceof C) {
  echo "o is of class C\n";
}
