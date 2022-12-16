<?php

function main() {
  $x = 2;
  var_dump($x);
  $y =& $x;
  var_dump($x);
  var_dump($y);
  //$y = 3;
}

main();
