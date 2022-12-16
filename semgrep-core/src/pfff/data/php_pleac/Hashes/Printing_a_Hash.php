# **********************************************************************
# Printing a Hash
# **********************************************************************
<?php
function pleac_Printing_a_Hash() {
// PHP offers, 'print_r', which prints hash contents in 'debug' form; it also
// works recursively, printing any contained arrays in similar form
//     Array
//     (
//         [key1] => value1
//         [key2] => value2
//         ...
//     )

print_r($hash);

// ------------

// Based on Perl example; non-recursive, so contained arrays not printed correctly
foreach($hash as $key => $value)
{
  echo "{$key} => $value\n";
}

// ----------------------------

// Sorted by keys

// 1. Sort the original hash
ksort($hash);

// 2. Extract keys, sort, traverse original in key order
$keys = array_keys($hash); sort($keys);

foreach($keys as $key)
{
  echo "{$key} => {$hash[$key]}\n";
}

// Sorted by values

// 1. Sort the original hash
asort($hash);

// 2. Extract values, sort, traverse original in value order [warning: finds
//    only first matching key in the case where duplicate values exist]
$values = array_values($hash); sort($values);

foreach($values as $value)
{
  echo $value . ' <= ' . array_search($value, $hash) . "\n";
}

}
?>
