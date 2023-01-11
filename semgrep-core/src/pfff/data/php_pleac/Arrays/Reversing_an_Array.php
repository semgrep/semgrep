# **********************************************************************
# Reversing an Array
# **********************************************************************
<?php
function pleac_Reversing_an_Array() {
$reversed = array_reverse($array);

// ----------------------------

foreach(array_reverse($array) as $item)
{
  ; // ... do something with '$item' ...
}

// ------------

for($i = count($array) - 1; $i >= 0; $i--)
{
  ; // ... do something with '$array[$i]' ...
}

// ----------------------------

sort($array);
$array = array_reverse($array);

// ------------

rsort($array);

}
?>
