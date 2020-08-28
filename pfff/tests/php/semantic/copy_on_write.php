<?php

$x = 1;
$y = $x;
$x = 2;
// this will print 1, cos PHP '=' is kinda like a deep_copy operator
// except that for performance reasons they do it lazily (when someone
// writes into it, as in an OS with fork's copy-on-write)
var_dump($y);

$x2 = 1;
$y2 =& $x2;
$x2 = 2;
// this will print 2
var_dump($y2);

$x3 = array(1,2,3);
$y3 = $x3;
$x3[0] = 42;
// the copy will be done even on big structures like array
var_dump($y3);
