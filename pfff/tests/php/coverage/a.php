<?php

function loop_a() {
  echo "a";
}

function a() {
  for($i = 1; $i < 100; $i++) {
    loop_a();
  }
}
