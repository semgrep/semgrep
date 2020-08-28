<?php

function foo($x) {
  var_dump($x);
}

// The interpreter will run those multiple funcalls with different contexts
// so it's kinda context-sensitive. The problem is that it's not really
// path sensitive (see if.php).
foo(42);
foo("bar");
