<?hh
// ERROR: match
$x = ($value)[io] ==> $value;
$y = ($value)[rand] ==> $value;
$z = ($value)[] ==> $value;
