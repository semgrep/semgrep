<?hh
// ERROR: match
$obj->bar<int>(1);
$obj->bar<string>(1);
$obj->bar(1);
$obj->other<int>(1);
