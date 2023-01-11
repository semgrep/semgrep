# **********************************************************************
# Traversing a Hash
# **********************************************************************
<?php
function pleac_Traversing_a_Hash() {
// Access keys and values
foreach($hash as $key => $value)
{
  ; // ...
}

// Access keys only
foreach(array_keys($hash) as $key)
{
  ; // ...
}

// Access values only
foreach($hash as $value)
{
  ; // ...
}

// ----------------------------

$food_colour = array('Apple' => 'red', 'Banana' => 'yellow',
                     'Lemon' => 'yellow', 'Carrot' => 'orange');

foreach($food_colour as $food => $colour)
{
  echo "{$food} is {$colour}\n";
}

foreach(array_keys($food_colour) as $food)
{
  echo "{$food} is {$food_colour[$food]}\n";
}

// ----------------------------

// 'countfrom' - count number of messages from each sender

$line = fgets(STDIN);

while (!feof(STDIN))
{
  if (preg_match('/^From: (.*)/', $line, $matches))
  {
    if (isset($from[$matches[1]]))
      $from[$matches[1]] += 1;
    else
      $from[$matches[1]] = 1;
  }

  $line = fgets(STDIN);
}

if (isset($from))
{
  echo "Senders:\n";
  foreach($from as $sender => $count) echo "{$sender} : {$count}\n";
}
else
{
  echo "No valid data entered\n";
}

}
?>
