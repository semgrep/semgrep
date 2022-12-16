<?php

// facebook extension
function foo() {
  await cached_result(123);
  $v = await gen_something();
  list($v1, $v2) = await gen_something_else();
}
