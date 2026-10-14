<?hh
// ERROR: match
$x = Vector {1, 2};
$y = Set {1, 2};
$z = Map {1 => 2};
$constructor = new Vector(1, 2);
// ERROR: match
$one = Vector {1};
// ERROR: match
$three = Vector {1, 2, 3};
$empty = Vector {};
