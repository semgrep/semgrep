<?php

$x = 2;
$y = -$x;
// should be a precise -2, there was a bug before and it was returning
// a Vabstr Tint
var_dump($y);
