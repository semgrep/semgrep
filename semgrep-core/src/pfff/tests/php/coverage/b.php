<?php

function loop_b() {
  echo "b";
}

function b() {
  for($i = 1; $i < 100; $i++) {
    loop_b();
  }
}
