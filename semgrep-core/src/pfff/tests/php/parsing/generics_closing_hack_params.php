<?hh

// C++ is famous for forcing people to add space between > to
// avoid ambiguities with the '>>' operator as in
// vector<vector<int> >.
// sphp currently uses some parsing hacks to allow the vector<vector<int>>
// syntax

function foo<A, B as X<int>>(A $x) {
  return null;
}
