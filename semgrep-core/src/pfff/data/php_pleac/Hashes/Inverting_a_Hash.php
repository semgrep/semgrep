# **********************************************************************
# Inverting a Hash
# **********************************************************************
<?php
function pleac_Inverting_a_Hash() {
// PHP offers the 'array_flip' function to perform the task of exchanging the keys / values
// of a hash i.e. invert or 'flip' a hash

$reverse = array_flip($hash);

// ----------------------------

$surname = array('Babe' => 'Ruth', 'Mickey' => 'Mantle');
$first_name = array_flip($surname);

echo "{$first_name['Mantle']}\n";

// ----------------------------

$argc == 2 || die("usage: {$argv[0]} food|colour\n");

$given = $argv[1];

$colour = array('Apple' => 'red', 'Banana' => 'yellow',
                'Lemon' => 'yellow', 'Carrot' => 'orange');

$food = array_flip($colour);

if (isset($colour[$given]))
  echo "{$given} is a food with colour: {$colour[$given]}\n";

if (isset($food[$given]))
  echo "{$food[$given]} is a food with colour: {$given}\n";

// ----------------------------

$food_colour = array('Apple' => 'red', 'Banana' => 'yellow',
                     'Lemon' => 'yellow', 'Carrot' => 'orange');

foreach($food_colour as $food => $colour)
{
  $foods_with_colour[$colour][] = $food;
}

$colour = 'yellow';
echo "foods with colour {$colour} were: " . join(' ', $foods_with_colour[$colour]) . "\n";

}
?>
