<?php
function f() {
  $a = 5;
  $c = 1;
  while ($c <= $a) {
    $c = $c + $c;
  }
  $a = $c - $a;
  $c = 0;
}
