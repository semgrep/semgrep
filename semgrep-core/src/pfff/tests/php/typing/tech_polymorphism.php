<?php

// this will be alpha -> alpha
function myid($x) {
  return $x;
}

// this will return an int
function context_influence() {
  $x = myid(1);
  return $x;
}

// this will return a bool
function context_influence2() {
  $x = myid(true);
  return $x;
}
