<?php

// PHP is case insensitive, which is bad, and scheck/cmf should enforce
// case sensitivity, but sgrep should not.

function test() {
  foo();
  foO();
  FoO();
}
