# **********************************************************************
# Appending One Array to Another
# **********************************************************************
<?php
function pleac_Appending_One_Array_to_Another() {
// PHP offers the 'array_merge' function to perform this task. Duplicate values are retained,
// but if arrays are numerically-indexed, resulting array is reindexed

$arr1 = array('c', 'a', 'b', 'd');
$arr2 = array('c', 'a', 'b', 'e');

$new = array_merge($arr1, $arr2);     // $new -> 'c', 'a', 'b', 'd', 'c', 'a', 'b', 'd'

// ----------------------------

$members = array('Time', 'Flies');
$initiates = array('An', 'Arrow');

$members = array_merge($members, $initiates);

// ------------

$members = array('Time', 'Flies');
$initiates = array('An', 'Arrow');

// 'array_splice' is the PHP equivalent to Perl's 'splice'
array_splice($members, 2, 0, array_merge(array('Like'), $initiates));
echo join(' ', $members) . "\n";

array_splice($members, 0, 1, array('Fruit'));
array_splice($members, -2, 2, array('A', 'Banana'));
echo join(' ', $members) . "\n";

}
?>
