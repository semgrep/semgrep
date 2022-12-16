<?php

// using {} instead of [] seems to be allowed
$o[1] = 42;
$o2{1} = 42;
var_dump($o);
var_dump($o2);

//according to
//http://www.php.net/manual/en/language.types.string.php#language.types.string.substr
// this is supposed to be used only for strings but it seems to work
// for any array.
