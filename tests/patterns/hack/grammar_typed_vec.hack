<?hh
// ERROR: match
$x = vec<int>[1, 2];
$y = vec<string>["a"];
$z = vec[1, 2];
// ERROR: match
$one = vec<int>[1];
