# **********************************************************************
# Changing Array Size
# **********************************************************************
<?php
function pleac_Changing_Array_Size() {
// AFAICT you cannot grow / shrink an array to an arbitrary size. However, you can:
// * Grow an array by appending an element using subscrip notation, or using
//   either 'array_unshift' or 'array_push' to add one or more elements

$arr[] = 'one';
array_unshift($arr, 'one', 'two', 'three');
array_push($arr, 'one', 'two', 'three');

// * Shrink an array by using 'unset' to remove one or more specific elements, or
//   either 'array_shift' or 'array_pop' to remove an element from the ends

unset($arr[$idx1], $arr[$idx2], $arr[$idx3]);
$item = array_shift($arr);
$item = array_pop($arr);

// ----------------------------

function what_about_the_array()
{
  global $people;

  echo 'The array now has ' . count($people) . " elements\n";
  echo 'The index value of the last element is ' . (count($people) - 1) . "\n";
  echo 'Element #3 is ' . $people[3] . "\n";
}

$people = array('Crosby', 'Stills', 'Nash', 'Young');
what_about_the_array();

array_pop($people);
what_about_the_array();

// Cannot, AFAICT, resize the array to an arbitrary size

}
?>
