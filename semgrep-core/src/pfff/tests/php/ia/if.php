<?php

// the current algorithm for the interpreter is not to manage
// branches independently and merge unify heaps after, but
// instead process branch sequentially and unify/merge/abstract
// as you go.

if(true) {
  $x = 42;
  //$x will be precise while processing the then branch ({42})
  var_dump($x);
} else {
  $x = 55;
  //$x will be abstract while processing the else {int} :(
  var_dump($x);
}
