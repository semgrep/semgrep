# **********************************************************************
# Testing for the Presence of a Key in a Hash
# **********************************************************************
<?php
function pleac_Testing_for_the_Presence_of_a_Key_in_a_Hash() {
// Returns TRUE on all existing entries with non-NULL values
if (isset($hash[$key]))
  ; // entry exists
else
  ; // no such entry

// ------------

// Returns TRUE on all existing entries regardless of attached value
if (array_key_exists($key, $hash))
  ; // entry exists
else
  ; // no such entry

// ----------------------------

$food_colour = array('Apple' => 'red', 'Banana' => 'yellow',
                     'Lemon' => 'yellow', 'Carrot' => 'orange');

foreach(array('Banana', 'Martini') as $name)
{
  if (isset($food_colour[$name]))
    echo "{$name} is a food.\n";
  else
    echo "{$name} is a drink.\n";
}

// ----------------------------

$age = array('Toddler' => 3, 'Unborn' => 0, 'Phantasm' => NULL);

foreach(array('Toddler', 'Unborn', 'Phantasm', 'Relic') as $thing)
{
  echo "{$thing}:";
  if (array_key_exists($thing, $age)) echo ' exists';
  if (isset($age[$thing])) echo ' non-NULL';
  if ($age[$thing]) echo ' TRUE';
  echo "\n";
}

}
?>
