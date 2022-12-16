# **********************************************************************
# Introduction
# **********************************************************************
<?php
function pleac_Introduction() {
$entry = stat('/bin/vi');
$entry = stat('/usr/bin');
$entry = stat($argv[1]);

// ------------

$entry = stat('/bin/vi');

$ctime = $entry['ctime'];
$size = $entry['size'];

// ----------------------------

// For the simple task of determining whether a file contains, text', a simple
// function that searches for a newline could be implemented. Not exactly
// foolproof, but very simple, low overhead, no installation headaches ...
function containsText($file)
{
  $status = FALSE;

  if (($fp = fopen($file, 'r')))
  {
    while (FALSE !== ($char = fgetc($fp)))
    {
      if ($char == "\n") { $status = TRUE; break; }
    }

    fclose($fp);
  }

  return $status;
}

// PHP offers the [currently experimental] Fileinfo group of functions to
// determine file types based on their contents / 'magic numbers'. This
// is functionality similar to the *NIX, 'file' utility. Note that it must
// first be installed using the PEAR utility [see PHP documentation]
function isTextFile($file)
{
  // Note: untested code, but I believe this is how it is supposed to work
  $finfo = finfo_open(FILEINFO_NONE);
  $status = (finfo_file($finfo, $file) == 'ASCII text');
  finfo_close($finfo);
  return $status;
}

// Alternatively, use the *NIX utility, 'file', directly
function isTextFile($file)
{
  return exec(trim('file -bN ' . escapeshellarg($file))) == 'ASCII text';
}

// ----

containsText($argv[1]) || die("File {$argv[1]} doesn't have any text in it\n");

isTextFile($argv[1]) || die("File {$argv[1]} doesn't have any text in it\n");

// ----------------------------

$dirname = '/usr/bin/';

($dirhdl = opendir($dirname)) || die("Couldn't open {$dirname}\n");

while (($file = readdir($dirhdl)) !== FALSE)
{
  printf("Inside %s is something called: %s\n", $dirname, $file);
}

closedir($dirhdl);

}
?>
