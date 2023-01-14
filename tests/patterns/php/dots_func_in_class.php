<?php

// ERROR:
function __destruct() {
    // do something
}

class Test {
  // ERROR:
  function __destruct() {
    // do something
  }

  function other() {
    // do something
  }
}

?>
