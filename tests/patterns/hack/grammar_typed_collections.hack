<?hh
// ERROR: match
$x = Vector<int> {1, 2};
$y = Vector<string> {"one", "two"};
$z = Set<int> {1, 2};
// ERROR: match
$one = Vector<int> {1};
// ERROR: match
$three = Vector<int> {1, 2, 3};
$empty = Vector<int> {};
