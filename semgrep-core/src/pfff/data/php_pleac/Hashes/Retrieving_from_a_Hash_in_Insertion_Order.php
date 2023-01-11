# **********************************************************************
# Retrieving from a Hash in Insertion Order
# **********************************************************************
<?php
function pleac_Retrieving_from_a_Hash_in_Insertion_Order() {
// Unless sorted, hash elements remain in the order of insertion. If care is taken to
// always add a new element to the end of the hash, then element order is the order
// of insertion. The following function, 'array_push_associative' [modified from original
// found at 'array_push' section of PHP documentation], does just that
function array_push_associative(&$arr)
{
  foreach (func_get_args() as $arg)
  {
    if (is_array($arg))
      foreach ($arg as $key => $value) { $arr[$key] = $value; $ret++; }
    else
      $arr[$arg] = '';
  }

  return $ret;
}

// ------------

$food_colour = array();

// Individual calls, or ...
array_push_associative($food_colour, array('Banana' => 'Yellow'));
array_push_associative($food_colour, array('Apple' => 'Green'));
array_push_associative($food_colour, array('Lemon' => 'Yellow'));

// ... one call, one array; physical order retained
// array_push_associative($food_colour, array('Banana' => 'Yellow', 'Apple' => 'Green', 'Lemon' => 'Yellow'));

print_r($food_colour);

echo "\nIn insertion order:\n";
foreach($food_colour as $food => $colour) echo "  {$food} => {$colour}\n";

$foods = array_keys($food_colour);

echo "\nStill in insertion order:\n";
foreach($foods as $food) echo "  {$food} => {$food_colour[$food]}\n";

}
?>
