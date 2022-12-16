# **********************************************************************
# Implementing a Circular List
# **********************************************************************
<?php
function pleac_Implementing_a_Circular_List() {
array_unshift($a1, array_pop($a1));  // last -> first
array_push($a1, array_shift($a1));   // first -> last

// ----------------------------

function grab_and_rotate(&$arr)
{
  $item = $arr[0];
  array_push($arr, array_shift($arr));
  return $item;
}

// ------------

$processes = array(1, 2, 3, 4, 5);

while (TRUE)
{
  $process = grab_and_rotate($processes);
  echo "Handling process {$process}\n";
  sleep(1);
}

}
?>
