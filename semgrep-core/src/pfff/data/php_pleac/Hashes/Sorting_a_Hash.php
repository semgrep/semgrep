# **********************************************************************
# Sorting a Hash
# **********************************************************************
<?php
function pleac_Sorting_a_Hash() {
// PHP implements a swag of sorting functions, most designed to work with numerically-indexed
// arrays. For sorting hashes, the 'key' sorting functions are required:
// * 'ksort', 'krsort', 'uksort'

// Ascending order
ksort($hash);

// Descending order [i.e. reverse sort]
krsort($hash);

// Comparator-based sort

function comparator($left, $right)
{
  // Compare left key with right key
  return $left > $right;
}

uksort($hash, 'comparator');

// ----------------------------

$food_colour = array('Apple' => 'red', 'Banana' => 'yellow',
                     'Lemon' => 'yellow', 'Carrot' => 'orange');

// ------------

ksort($food_colour);

foreach($food_colour as $food => $colour)
{
  echo "{$food} is {$colour}\n";
}

// ------------

uksort($food_colour, create_function('$left, $right', 'return $left > $right;'));

foreach($food_colour as $food => $colour)
{
  echo "{$food} is {$colour}\n";
}

}
?>
