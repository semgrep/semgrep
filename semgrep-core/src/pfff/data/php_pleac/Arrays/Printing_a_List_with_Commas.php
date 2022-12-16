# **********************************************************************
# Printing a List with Commas
# **********************************************************************
<?php
function pleac_Printing_a_List_with_Commas() {
function commify_series($list)
{
  $n = str_word_count($list); $series = str_word_count($list, 1);

  if ($n == 0) return NULL;
  if ($n == 1) return $series[0];
  if ($n == 2) return $series[0] . ' and ' . $series[1];

  return join(', ', array_slice($series, 0, -1)) . ', and ' . $series[$n - 1];
}

// ------------

echo commify_series('red') . "\n";
echo commify_series('red yellow') . "\n";
echo commify_series('red yellow green') . "\n";

$mylist = 'red yellow green';
echo 'I have ' . commify_series($mylist) . " marbles.\n";

// ----------------------------

function commify_series($arr)
{
  $n = count($arr); $sepchar = ',';

  foreach($arr as $str)
  {
    if (strpos($str, ',') === false) continue;
    $sepchar = ';'; break;
  }

  if ($n == 0) return NULL;
  if ($n == 1) return $arr[0];
  if ($n == 2) return $arr[0] . ' and ' . $arr[1];

  return join("{$sepchar} ", array_slice($arr, 0, -1)) . "{$sepchar} and " . $arr[$n - 1];
}

// ------------

$lists = array(
  array('just one thing'),
  split(' ', 'Mutt Jeff'),
  split(' ', 'Peter Paul Mary'),
  array('To our parents', 'Mother Theresa', 'God'),
  array('pastrami', 'ham and cheese', 'peanut butter and jelly', 'tuna'),
  array('recycle tired, old phrases', 'ponder big, happy thoughts'),
  array('recycle tired, old phrases', 'ponder big, happy thoughts', 'sleep and dream peacefully'));

foreach($lists as $arr)
{
  echo 'The list is: ' . commify_series($arr) . ".\n";
}

}
?>
