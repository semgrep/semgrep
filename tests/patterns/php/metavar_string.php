<?php

function test() {
  //ERROR: match
  foo("a string");
  //ERROR: match
  foo("\"escaped string\"");
  //TODO? should match too?
  foo(`a string too`);
  // this nope
  foo(1);
}
