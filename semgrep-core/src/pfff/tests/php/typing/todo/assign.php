<?php

function foo() {
  $i = 1;
  $j = $i;
  $i = "foo";
  return $j;
}
