<?php

// soft type hint, a hphp extension
function test(@int $x) {
  echo $x;
}

// This will not fatal
test("foo");

function test2(int $x) {
  echo $x;
}

// This will fatal
test2("foo");



function test3(): @int {
  return 'hi';
}