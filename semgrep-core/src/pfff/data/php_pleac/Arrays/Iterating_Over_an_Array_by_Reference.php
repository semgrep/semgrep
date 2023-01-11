# **********************************************************************
# Iterating Over an Array by Reference
# **********************************************************************
<?php
function pleac_Iterating_Over_an_Array_by_Reference() {
// Conventional 'read-only' access
foreach($array as $item)
{
  ; // Can access, but not update, array element referred to by '$item'
}

// ----

// '&' makes '$item' a reference
foreach($array as &$item)
{
  ; // Update array element referred to by '$item'
}

// ------------

$arraylen = count($array);

for($i = 0; $i < $arraylen; $i++)
{
  ; // '$array' is updateable via subscript notation
}

// ----------------------------

$fruits = array('Apple', 'Raspberry');

foreach($fruits as &$fruit)
{
  echo "{$fruit} tastes good in a pie.\n";
}

$fruitlen = count($fruits);

for($i = 0; $i < $fruitlen; $i++)
{
  echo "{$fruits[$i]} tastes good in a pie.\n";
}

// ----------------------------

$rogue_cats = array('Blackie', 'Goldie', 'Silkie');

// Take care to assign reference to '$rogue_cats' array via '=&'
$namelist['felines'] =& $rogue_cats;

// Take care to make '$cat' a reference via '&$' to allow updating
foreach($namelist['felines'] as &$cat)
{
  $cat .= ' [meow]';
}

// Via array reference
foreach($namelist['felines'] as $cat)
{
  echo "{$cat} purrs hypnotically.\n";
}

echo "---\n";

// Original array
foreach($rogue_cats as $cat)
{
  echo "{$cat} purrs hypnotically.\n";
}

}
?>
