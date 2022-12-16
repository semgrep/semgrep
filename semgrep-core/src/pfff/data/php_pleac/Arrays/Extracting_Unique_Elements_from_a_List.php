# **********************************************************************
# Extracting Unique Elements from a List
# **********************************************************************
<?php
function pleac_Extracting_Unique_Elements_from_a_List() {
// PHP offers the 'array_unique' function to perform this task. It works with both keyed,
// and numerically-indexed arrays; keys / indexes are preserved; use of 'array_values'
// is recommended to reindex numerically-indexed arrays since there will likely be missing
// indexes

// Remove duplicate values
$unique = array_unique($array);

// Remove duplicates, and reindex [for numerically-indexed arrays only]
$unique = array_values(array_unique($array));

// or use:
$unique = array_keys(array_flip($array));

// ----------------------------

// Selected Perl 'seen' examples
foreach($list as $item)
{
  if (!isset($seen[$item]))
  {
    $seen[$item] = TRUE;
    $unique[] = $item;
  }
}

// ------------

foreach($list as $item)
{
  $seen[$item] || (++$seen[$item] && ($unique[] = $item));
}

// ------------

function some_func($item)
{
  ; // Do something with '$item'
}

foreach($list as $item)
{
  $seen[$item] || (++$seen[$item] && some_func($item));
}

// ----------------------------

foreach(array_slice(preg_split('/\n/', `who`), 0, -1) as $user_entry)
{
  $user = preg_split('/\s/', $user_entry);
  $ucnt[$user[0]]++;
}

ksort($ucnt);

echo "users logged in:\n";

foreach($ucnt as $user => $cnt)
{
  echo "\t{$user} => {$cnt}\n";
}

}
?>
