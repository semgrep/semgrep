<?php

// Concrete value and variables to concrete values are
// represented differently (which implies to do many
// Ptr.get in the code)

function main() {
  $x = 1;
  var_dump($x);
  var_dump(2);
  var_dump(2 + $x);
}

main();
