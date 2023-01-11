# **********************************************************************
# Processing All Files in a Directory Recursively
# **********************************************************************
<?php
function pleac_Processing_All_Files_in_a_Directory_Recursively() {
// Recursive directory traversal function and helper: traverses a directory tree
// applying a function [and a variable number of accompanying arguments] to each
// file

class Accumulator
{
  public $value;
  public function __construct($start_value) { $this->value = $start_value; }
}

// ------------

function process_directory_($op, $func_args)
{
  if (is_dir($func_args[0]))
  {
    $current = $func_args[0];
    foreach(scandir($current) as $entry)
    {
      if ($entry == '.' || $entry == '..') continue;
      $func_args[0] = $current . '/' . $entry;
      process_directory_($op, $func_args);
    }
  }
  else
  {
    call_user_func_array($op, $func_args);
  }
}

function process_directory($op, $dir)
{
  if (!is_dir($dir)) return FALSE;
  $func_args = array_slice(func_get_args(), 1);
  process_directory_($op, $func_args);
  return TRUE;
}

// ----------------------------

$dirlist = array('/tmp/d1', '/tmp/d2', '/tmp/d3');

// Do something with each directory in the list
foreach($dirlist as $dir)
{
  ;
  // Delete directory [if empty]     -> rmdir($dir);
  // Make it the 'current directory' -> chdir($dir);
  // Get list of files it contains   -> $filelist = scandir($dir);
  // Get directory metadata          -> $ds = stat($dir);
}

// ------------

$dirlist = array('/tmp/d1', '/tmp/d2', '/tmp/d3');

function pf($path)
{
  // ... do something to the file or directory ...
  printf("%s\n", $path);
}

// For each directory in the list ...
foreach($dirlist as $dir)
{
  // Is this a valid directory ?
  if (!is_dir($dir)) { printf("%s does not exist\n", $dir); continue; }

  // Ok, so get all the directory's entries
  $filelist = scandir($dir);

  // An 'empty' directory will contain at least two entries: '..' and '.'
  if (count($filelist) == 2) { printf("%s is empty\n", $dir); continue; }

  // For each file / directory in the directory ...
  foreach($filelist as $file)
  {
    // Ignore '..' and '.' entries
    if ($file == '.' || $file == '..') continue;

    // Apply function to process the file / directory
    pf($dir . '/' . $file);
  }
}

// ----------------------------

function accum_filesize($file, $accum)
{
  is_file($file) && ($accum->value += filesize($file));
}

// ------------

// Verify arguments ...
$argc == 2 || die("usage: {$argv[0]} dir\n");
$dir = $argv[1];

is_dir($dir) || die("{$dir} does not exist / not a directory\n");

// Collect data [use an object to accumulate results]
$dirsize = new Accumulator(0);
process_directory('accum_filesize', $dir, $dirsize);

// Report results
printf("%s contains %d bytes\n", $dir, $dirsize->value);

// ----------------------------

function biggest_file($file, $accum)
{
  if (is_file($file))
  {
    $fs = filesize($file);
    if ($accum->value[1] < $fs) { $accum->value[0] = $file; $accum->value[1] = $fs; }
  }
}

// ------------

// Verify arguments ...
$argc == 2 || die("usage: {$argv[0]} dir\n");
$dir = $argv[1];

is_dir($dir) || die("{$dir} does not exist / not a directory\n");

// Collect data [use an object to accumulate results]
$biggest = new Accumulator(array('', 0));
process_directory('biggest_file', $dir, $biggest);

// Report results
printf("Biggest file is %s containing %d bytes\n", $biggest->value[0], $biggest->value[1]);

// ----------------------------

function youngest_file($file, $accum)
{
  if (is_file($file))
  {
    $fct = filectime($file);
    if ($accum->value[1] > $fct) { $accum->value[0] = $file; $accum->value[1] = $fct; }
  }
}

// ------------

// Verify arguments ...
$argc == 2 || die("usage: {$argv[0]} dir\n");
$dir = $argv[1];

is_dir($dir) || die("{$dir} does not exist / not a directory\n");

// Collect data [use an object to accumulate results]
$youngest = new Accumulator(array('', 2147483647));
process_directory('youngest_file', $dir, $youngest);

// Report results
printf("Youngest file is %s dating %s\n", $youngest->value[0], date(DATE_ATOM, $youngest->value[1]));

}
?>
