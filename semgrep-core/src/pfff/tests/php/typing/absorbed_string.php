<?php

//CONFIG: TODO for strict type checking we may want to change that

// In PHP one can concat int and strings without
// doing anything. Ints are castable in strings implicitly
// So strings are kinda of a supertype to int, and so are "absorbed"
function foo() {
  if(1) {
    $x = 1;
  } else {
    $x = "foo";
  }
  // the type will be string here, not (string | int)
  return $x;
}
