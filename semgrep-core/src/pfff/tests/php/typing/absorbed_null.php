<?php

//CONFIG: TODO for null analysis we may want to change that

// : unit -> int, null is absorbed :(
function foo() {
  $x = null;
  $x = 1;
  return $x;
}
