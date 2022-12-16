<?php

$a = 1 or 2;
var_dump($a);
$b = false or 2;
var_dump($b);

$b = (false or 2);
var_dump($b);
