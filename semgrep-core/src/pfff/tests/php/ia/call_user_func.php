<?php

function foo($x) {
  var_dump($x);
}

function bar($x) {
  var_dump($x);
}

$t = 'foo';
// this one will be ok
call_user_func($t, 5);

//APPROX:
// this one will not because $t will have been generalized to a Tstring
//$t = 'bar';
//call_user_func($t, 6);

// this is ok because it's another variable
$t2 = 'bar';
call_user_func($t2, 6);
