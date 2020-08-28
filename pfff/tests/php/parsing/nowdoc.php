<?php

// nowdoc were introduced in PHP 5.3
// See http://www.php.net/manual/en/language.types.string.php#language.types.string.syntax.nowdoc

function foo() {

  $x = 1;
// note that strings are _not_ interpolated inside nowdoc
  return <<<'ENDGETTER'
    this is $x
ENDGETTER;

}

//   'this is $x'
echo foo();
