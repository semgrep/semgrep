# **********************************************************************
# Processing Multiple Elements of an Array
# **********************************************************************
<?php
function pleac_Processing_Multiple_Elements_of_an_Array() {
// Array elements can be deleted using 'unset'; removing several elements would require applying
// 'unset' several times, probably in a loop. However, they would most likely also need to be
// reindexed, so a better approach would be to use 'array_slice' which avoids explicit looping.
// Where elements need to be removed, and those elements also returned, it is probably best to
// combine both operations in a function. This is the approach taken here in implementing both
// 'shiftN' and 'popN', and it is these functions that are used in the examples

function popN(&$arr, $n)
{
  $ret = array_slice($arr, -($n), $n);
  $arr = array_slice($arr, 0, count($arr) - $n);
  return $ret;
}

function shiftN(&$arr, $n)
{
  $ret = array_slice($arr, 0, $n);
  $arr = array_slice($arr, $n);
  return $ret;
}

// ------------

// Remove $n elements from the front of $array; return them in $fron
$front = shiftN($array, $n);

// Remove $n elements from the end of $array; return them in $end
$end = popN($array, $n);

// ------------

$friends = array('Peter', 'Paul', 'Mary', 'Jim', 'Tim');

list($this_, $that) = shiftN($friends, 2);

echo "{$this_} {$that}\n";

// ------------

$beverages = array('Dew', 'Jolt', 'Cola', 'Sprite', 'Fresca');

$pair = popN($beverages, 2);

echo join(' ', $pair) . "\n";

}
?>
