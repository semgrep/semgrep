<?php

function foo() {

  $x = 1;
// note that strings are interpolated inside heredoc
  return <<<ENDGETTER
    this is $x
ENDGETTER;

}

// this is 1
echo foo();
