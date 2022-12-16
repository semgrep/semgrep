# **********************************************************************
# Computing Union, Intersection, or Difference of Unique Lists
# **********************************************************************
<?php
function pleac_Computing_Union__Intersection__or_Difference_of_Unique_Lists() {
// PHP offers a number of array-based 'set operation' functions:
// * union:        array_unique(array_merge(...))
// * intersection: array_intersect and family
// * difference:   array_diff and family
// which may be used for this type of task. Also, if returned arrays need to be
// reindexed, 'array_slice($array, 0)', or 'array_values($array)' are useful

$a = array(1, 3, 5, 6, 7, 8);
$b = array(2, 3, 5, 7, 9);

$union = array_values(array_unique(array_merge($a, $b))); // 1, 2, 3, 5, 6, 7, 8, 9
$isect = array_values(array_intersect($a, $b));           // 3, 5, 7
$diff = array_values(array_diff($a, $b));                 // 1, 8

}
?>
