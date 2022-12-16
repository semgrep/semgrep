<?php

$array = array(1, 2, array(3, 4), 5);

list($a, $b, list($c, $d), $e) = $array;
var_dump(array($a, $b, $c, $d, $e));
