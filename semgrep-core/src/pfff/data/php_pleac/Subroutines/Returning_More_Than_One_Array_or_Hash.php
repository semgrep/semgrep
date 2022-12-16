# **********************************************************************
# Returning More Than One Array or Hash
# **********************************************************************
<?php
function pleac_Returning_More_Than_One_Array_or_Hash() {
// Multiple return values are possible via packing a set of values within a
// numerically-indexed array and using 'list' to extract them

function some_func() { return array(array(1, 2, 3), array('a' => 1, 'b' => 2)); }

// ------------

list($arr, $hash) = some_func();

// ----------------------------

function some_func(&$arr, &$hash) { return array($arr, $hash); }

// ------------

$arrin = array(1, 2, 3); $hashin = array('a' => 1, 'b' => 2);

list($arr, $hash) = some_func($arrin, $hashin);

}
?>
