# **********************************************************************
# Randomizing an Array
# **********************************************************************
<?php
function pleac_Randomizing_an_Array() {
// PHP offers the 'shuffle' function to perform this task

$arr = array(1, 2, 3, 4, 5, 6, 7, 8, 9);

shuffle($arr);

echo join(' ', $arr) . "\n";

// ----------------------------

// Perl example equivalents
function fisher_yates_shuffle(&$a)
{
  $size = count($a) - 1;

  for($i = $size; $i >= 0; $i--)
  {
    if (($j = rand(0, $i)) != $i)
      list($a[$i], $a[$j]) = array($a[$j], $a[$i]);
  }
}

function naive_shuffle(&$a)
{
  $size = count($a);

  for($i = 0; $i < $size; $i++)
  {
    $j = rand(0, $size - 1);
    list($a[$i], $a[$j]) = array($a[$j], $a[$i]);
  }
}

// ------------

$arr = array(1, 2, 3, 4, 5, 6, 7, 8, 9);

fisher_yates_shuffle($arr);
echo join(' ', $arr) . "\n";

naive_shuffle($arr);
echo join(' ', $arr) . "\n";

}
?>
