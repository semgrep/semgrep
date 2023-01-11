# **********************************************************************
# Finding Elements in One Array but Not Another
# **********************************************************************
<?php
function pleac_Finding_Elements_in_One_Array_but_Not_Another() {
// PHP offers the 'array_diff' and 'array_diff_assoc' functions to perform this task. Same
// points as made about 'array_unique' apply here also

$a = array('c', 'a', 'b', 'd');
$b = array('c', 'a', 'b', 'e');

$diff = array_diff($a, $b);                 // $diff -> [3] 'd'
$diff = array_diff($b, $a);                 // $diff -> [3] 'e'

// Numerically-indexed array, reindexed
$diff = array_values(array_diff($a, $b));   // $diff -> [0] 'd'
$diff = array_values(array_diff($b, $a));   // $diff -> [0] 'e'

// ----------------------------

// 1st Perl 'seen' example only

$a = array('k1' => 11, 'k2' => 12, 'k4' => 14);
$b = array('k1' => 11, 'k2' => 12, 'k3' => 13);

foreach($b as $item => $value) { $seen[$item] = 1; }

// Stores key only e.g. $aonly[0] contains 'k4', same as Perl example
foreach($a as $item => $value) { if (!$seen[$item]) $aonly[] = $item; }

// Stores key and value e.g. $aonly['k4'] contains 14, same entry as in $a
foreach($a as $item => $value) { if (!$seen[$item]) $aonly[$item] = $value; }

// ----------------------------

// Conventional way: $hash = array('key1' => 1, 'key2' => 2);

$hash['key1'] = 1;
$hash['key2'] = 2;

$hash = array_combine(array('key1', 'key2'), array(1, 2));

// ------------

$seen = array_slice($b, 0);

$seen = array_combine(array_keys($b), array_fill(0, count($b), 1));

}
?>
