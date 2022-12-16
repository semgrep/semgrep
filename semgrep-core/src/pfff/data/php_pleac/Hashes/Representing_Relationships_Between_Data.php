# **********************************************************************
# Representing Relationships Between Data
# **********************************************************************
<?php
function pleac_Representing_Relationships_Between_Data() {
$father = array('Cain' => 'Adam', 'Abel' => 'Adam', 'Seth' => 'Adam', 'Enoch' => 'Cain',
                'Irad' => 'Enoch', 'Mehujael' => 'Irad', 'Methusael'=> 'Mehujael',
                'Lamech' => 'Methusael', 'Jabal' => 'Lamech', 'Jubal' => 'Lamech',
                'Tubalcain' => 'Lamech', 'Enos' => 'Seth');

// ------------

$name = trim(fgets(STDIN));

while (!feof(STDIN))
{
  while (TRUE)
  {
    echo "$name\n";

    // Can use either:
    if (!isset($father[$name])) break;
    $name = $father[$name];

    // or:
    // if (!key_exists($name, $father)) break;
    // $name = $father[$name];

    // or combine the two lines:
    // if (!($name = $father[$name])) break;
  }

  echo "\n";
  $name = trim(fgets(STDIN));
}

// ----------------------------

define(SEP, ' ');

foreach($father as $child => $parent)
{
  if (!$children[$parent])
    $children[$parent] = $child;
  else
    $children[$parent] .= SEP . $child;
}

$name = trim(fgets(STDIN));

while (!feof(STDIN))
{
  echo $name . ' begat ';

  if (!$children[$name])
    echo "Nothing\n";
  else
    echo str_replace(SEP, ', ', $children[$name]) . "\n";

  $name = trim(fgets(STDIN));
}

// ----------------------------

define(SEP, ' ');

$files = array('/tmp/a', '/tmp/b', '/tmp/c');

foreach($files as $file)
{
  if (!is_file($file)) { echo "Skipping {$file}\n"; continue; }
  if (!($fh = fopen($file, 'r'))) { echo "Skipping {$file}\n"; continue; }

  $line = fgets($fh);

  while (!feof($fh))
  {
    if (preg_match('/^\s*#\s*include\s*<([^>]+)>/', $line, $matches))
    {
      if (isset($includes[$matches[1]]))
        $includes[$matches[1]] .= SEP . $file;
      else
        $includes[$matches[1]] = $file;
    }

    $line = fgets($fh);
  }

  fclose($fh);
}

print_r($includes);

}
?>
