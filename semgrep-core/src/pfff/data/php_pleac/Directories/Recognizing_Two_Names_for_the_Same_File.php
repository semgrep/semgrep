# **********************************************************************
# Recognizing Two Names for the Same File
# **********************************************************************
<?php
function pleac_Recognizing_Two_Names_for_the_Same_File() {
function makeDevInodePair($filename)
{
  if (!($fs = @stat($filename))) return FALSE;
  return strval($fs['dev'] . $fs['ino']);
}

// ------------

function do_my_thing($filename)
{
  // Using a global variable to mimic Perl example, but could easily have passed
  // '$seen' as an argument
  global $seen;

  $devino = makeDevInodePair($filename);

  // Process $filename if it has not previously been seen, else just increment
  if (!isset($seen[$devino]))
  {
    // ... process $filename ...

    // Set initial count
    $seen[$devino] = 1;
  }
  else
  {
    // Otherwise, just increment the count
    $seen[$devino] += 1;
  }
}

// ----

// Simple example
$seen = array();

do_my_thing('/tmp/old');
do_my_thing('/tmp/old');
do_my_thing('/tmp/old');
do_my_thing('/tmp/new');

foreach($seen as $devino => $count)
{
  echo "{$devino} -> {$count}\n";
}

// ------------

// A variation on the above avoiding use of global variables, and illustrating use of
// easily-implemented 'higher order' techniques

// Helper function loosely modelled on, 'array_reduce', but using an array as
// 'accumulator', which is returned on completion
function array_update($arr, $lambda, $updarr)
{
  foreach($arr as $key) $lambda($updarr, $key);
  return $updarr;
}

function do_my_thing(&$seen, $filename)
{
  if (!array_key_exists(($devino = makeDevInodePair($filename)), $seen))
  {
    // ... processing $filename ...

    // Update $seen
    $seen[$devino] = 1;
  }
  else
  {
    // Update $seen
    $seen[$devino] += 1;
  }
}

// ----

// Simple example
$files = array('/tmp/old', '/tmp/old', '/tmp/old', '/tmp/new');

// Could do this ...
$seen = array();
array_update($files, 'do_my_thing', &$seen);

// or this:
$seen = array_update($files, 'do_my_thing', array());

// or a 'lambda' could be used:
array_update($files,
             create_function('$seen, $filename', '... code not shown ...'),
             &$seen);

foreach($seen as $devino => $count)
{
  echo "{$devino} -> {$count}\n";
}

// ----------------------------

$files = glob('/tmp/*');

define(SEP, ';');
$seen = array();

foreach($files as $filename)
{
  if (!array_key_exists(($devino = makeDevInodePair($filename)), $seen))
    $seen[$devino] = $filename;
  else
    $seen[$devino] = $seen[$devino] . SEP . $filename;
}

$devino = array_keys($seen);
sort($devino);

foreach($devino as $key)
{
  echo $key . ':';
  foreach(split(SEP, $seen[$key]) as $filename) echo ' ' . $filename;
  echo "\n";
}

}
?>
