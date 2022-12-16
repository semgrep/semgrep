<?php

function test() {

for ($i = 0; $i < 3; $i++) {
  if($i === 1) {
    continue;
  }
  var_dump($i);
}

}

test();
