# **********************************************************************
# Skipping Selected Return Values
# **********************************************************************
<?php
function pleac_Skipping_Selected_Return_Values() {
// The 'list' keyword [looks like a function but is actually a special language
// construct] may be used to perform multiple assignments from a numerically
// indexed array of values, and offers the added bonus of being able to skip
// assignment of one, or more, of those values

function func() { return array(3, 6, 9); }

// ------------

list($a, $b, $c) = array(6, 7, 8);

// Provided 'func' returns an numerically-indexed array, the following
// multiple assignment will work
list($a, $b, $c) = func();

// Any existing variables no longer wanted would need to be 'unset'
unset($b);

// As above, but second element of return array discarded
list($a,,$c) = func();

// ----------------------------

// Care needed to ensure returned array is numerically-indexed
list($dev, $ino,,,$uid) = array_slice(array_values(stat($filename)), 0, 13);

}
?>
