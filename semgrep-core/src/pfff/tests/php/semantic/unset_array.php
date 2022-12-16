<?php

$x = array(1,2,3);
var_dump($x);
unset($x[0], $x[1]);
var_dump($x);
