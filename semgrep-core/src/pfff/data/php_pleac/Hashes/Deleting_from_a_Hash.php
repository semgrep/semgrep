# **********************************************************************
# Deleting from a Hash
# **********************************************************************
<?php
function pleac_Deleting_from_a_Hash() {
// Remove one, or more, hash entries
unset($hash[$key]);

unset($hash[$key1], $hash[$key2], $hash[$key3]);

// Remove entire hash
unset($hash);

// ----------------------------

function print_foods()
{
  // Perl example uses a global variable
  global $food_colour;

  $foods = array_keys($food_colour);

  echo 'Foods:';
  foreach($foods as $food) echo " {$food}";

  echo "\nValues:\n";
  foreach($foods as $food)
  {
    $colour = $food_colour[$food];

    if (isset($colour))
      echo "  {$colour}\n";
    else
      echo "  nullified or removed\n";
  }
}

// ------------

$food_colour = array('Apple' => 'red', 'Banana' => 'yellow',
                     'Lemon' => 'yellow', 'Carrot' => 'orange');

echo "Initially:\n"; print_foods();

// Nullify an entry
$food_colour['Banana'] = NULL;
echo "\nWith 'Banana' nullified\n";
print_foods();

// Remove an entry
unset($food_colour['Banana']);
echo "\nWith 'Banana' removed\n";
print_foods();

// Destroy the hash
unset($food_colour);

}
?>
