# **********************************************************************
# Merging Hashes
# **********************************************************************
<?php
function pleac_Merging_Hashes() {
// PHP offers the 'array_merge' function for this task [a related function, 'array_combine',
// may be used to create a hash from an array of keys, and one of values, respectively]

// Merge two, or more, arrays
$merged = array_merge($a, $b, $c);

// Create a hash from array of keys, and of values, respectively
$hash = array_combine($keys, $values);

// ------------

// Can always merge arrays manually
foreach(array($h1, $h2, $h3) as $hash)
{
  foreach($hash as $key => $value)
  {
    // If same-key values differ, only latest retained
    $merged[$key] = $value;

    // Do this to append values for that key
    // $merged[$key][] = $value;
  }
}

// ----------------------------

$food_colour = array('Apple' => 'red', 'Banana' => 'yellow',
                     'Lemon' => 'yellow', 'Carrot' => 'orange');

$drink_colour = array('Galliano' => 'yellow', 'Mai Tai' => 'blue');

// ------------

$ingested_colour = array_merge($food_colour, $drink_colour);

// ------------

$substance_colour = array();

foreach(array($food_colour, $drink_colour) as $hash)
{
  foreach($hash as $substance => $colour)
  {
    if (array_key_exists($substance, $substance_colour))
    {
      echo "Warning {$substance_colour[$substance]} seen twice. Using first definition.\n";
      continue;
    }
    $substance_colour[$substance] = $colour;
  }
}

}
?>
