<?php

// facebook extension
function foo() {
  yield 1;
  $v = yield 2;
  list($v1, $v2) = yield 4;

  yield break;
}
