<?php

$foo = array(42);
$unrelated = array('beep', &$foo[0], 'boop');
$bar = $foo;
if (rand() & 1) {
  unset($unrelated);
}
$foo[0] = 'i am shared';
var_dump($bar);
