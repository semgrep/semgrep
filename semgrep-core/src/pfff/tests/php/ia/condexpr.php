<?php

var_dump(true ? 45 : "foo");
$x = 45;
$y = "foo";
//TODO: why this is &1{choice(...)} and the top one is just choice(...)
// and why suddently $x and $y become choice() too?
var_dump(true ? $x : $y);
