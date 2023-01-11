# **********************************************************************
# Renaming Files
# **********************************************************************
<?php
function pleac_Renaming_Files() {
$filepairs = array('x.txt' => 'x2.txt', 'y.txt' => 'y.doc', 'zxc.txt' => 'cxz.txt');

foreach($filepairs as $oldfile => $newfile)
{
  @rename($oldfile, $newfile) || fwrite(STDERR, sprintf("Could not rename %s to %s\n", $oldfile, $newfile));
}

// ----------------------------

// Call a system command utility via, 'exec'. *NIX-specific example. Could check whether,
// 'exec', succeeded, but checking whether a renamed file exists after the operation might
// be a better approach

$oldfile = '/tmp/old'; $newfile = '/tmp/new';

is_file($newfile) && unlink($newfile);

exec(trim('mv --force ' . escapeshellarg($oldfile) . ' ' . escapeshellarg($newfile)));

is_file($oldfile) || die("Problem renaming file {$oldfile} to file {$newfile}\n");

// For other operating systems just change:
// * filenames
// * command being 'exec'ed
// as the rest of the code is platform independant

// ----------------------------

// A modified implementation of Larry's Filename Fixer. Rather than passing
// a single expression, a 'from' regexp is passed; each match in the file
// name(s) is changed to the value of 'to'. It otherwise behaves the same
//

$argc > 2 || die("usage: rename from to [file ...]\n");

$from = $argv[1];
$to = $argv[2];

if (count(($argv = array_slice($argv, 3))) < 1)
  while (!feof(STDIN)) $argv[] = substr(fgets(STDIN), 0, -1);

foreach($argv as $file)
{
  $was = $file;
  $file = ereg_replace($from, $to, $file);

  if (strcmp($was, $file) != 0)
    @rename($was, $file) || fwrite(STDERR, sprintf("Could not rename %s to %s\n", $was, $file));
}

}
?>
