# **********************************************************************
# Passing Arrays and Hashes by Reference
# **********************************************************************
<?php
function pleac_Passing_Arrays_and_Hashes_by_Reference() {
// In PHP all items exist as 'memory references' [i.e. non-modifiable pointers],
// so when passing an item as a function argument, or returning an item from
// a function, it is this 'memory reference' that is passed, and *not* the
// contents of that item. Should several references to an item exist [e.g. if
// passed to a function then at least two such references would exist in
// different scopes] they would all be refering to the same copy of the item.
// However, if an attempt is made to alter the item is made, a copy is made
// and it is the copy that is altered, leaving the original intact.
//
// The PHP reference mechanism is used to selectively prevent this behaviour,
// and ensure that if a change is made to an item that no copy is made, and that
// it is the original item that is changed. Importantly, there is no efficiency
// gain from passing function parameters using references if the parameter item
// is not altered.

// A copy of the item referred to by, '$arr', is made, and altered; original
// remains intact
function array_by_value($arr)
{
  $arr[0] = 7;
  echo $arr[0] . "\n";
}

// No copy is made; original item referred to by, '$arr', is altered
function array_by_ref(&$arr)
{
  $arr[0] = 7;
  echo $arr[0] . "\n";
}

// ------------

$arr = array(1, 2, 3);

echo $arr[0] . "\n";         // output: 1
array_by_value($arr);        // output: 7
echo $arr[0] . "\n";         // output: 1

$arr = array(1, 2, 3);

echo $arr[0] . "\n";         // output: 1
array_by_ref($arr);          // output: 7
echo $arr[0] . "\n";         // output: 7

// ----------------------------

// Since, 'add_vecpair', does not attempt to alter either, '$x' or '$y', it makes
// no difference whether they are 'passed by value', or 'passed by reference'
function add_vecpair($x, $y)
{
  $r = array();
  $length = count($x);
  for($i = 0; $i < $length; $i++) $r[$i] = $x[$i] + $y[$i];
  return $r;
}

// ...
count($arr1) == count($arr2) || die("usage: add_vecpair ARR1 ARR2\n");

// 'passed by value'
$arr3 = add_vecpair($arr1, $arr2);

// 'passed by reference' [also possible to override default 'passed by value'
// if required]
$arr3 = add_vecpair(&$arr1, &$arr2);

}
?>
