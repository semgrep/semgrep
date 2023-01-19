<?php

function test() {
  // MATCH:
  $x->bar();
  // MATCH:
  $x->foo()->bar();

  // ok:
  $z->bar();
  $z->foo()->bar();
}
