<?php

//sgrep -lang phpfuzzy -e 'function $X() { return array($Y); }' demos/

function foo() {
  return array(array(1,2));
}
