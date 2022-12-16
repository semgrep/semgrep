# **********************************************************************
# Processing All Files in a Directory
# **********************************************************************
<?php
function pleac_Processing_All_Files_in_a_Directory() {
// Conventional POSIX-like approach to directory traversal
$dirname = '/usr/bin/';

($dirhdl = opendir($dirname)) || die("Couldn't open {$dirname}\n");

while (($file = readdir($dirhdl)) !== FALSE)
{
  ; // ... do something with $dirname/$file
    // ...
}

closedir($dirhdl);

// ------------

// Newer [post PHP 4], 'applicative' approach - an array of filenames is
// generated that may be processed via external loop ...

$dirname = '/usr/bin/';

foreach(scandir($dirname) as $file)
{
  ; // ... do something with $dirname/$file
    // ...
}

// .. or, via callback application, perhaps after massaging by one of the
// 'array' family of functions [also uses, 'array_update', from earlier section]

$newlist = array_update(array_reverse(scandir($dirname)),
                        create_function('$filelist, $file',  ' ; '),
                        array());

// And don't forget that the old standby, 'glob', that returns an array of
// paths filtered using the Bourne Shell-based wildcards, '?' and '*', is
// also available

foreach(glob($dirname . '*') as $path)
{
  ; // ... do something with $path
    // ...
}

// ----------------------------

// Uses, 'isTextFile', from an earlier section
$dirname = '/usr/bin/';

echo "Text files in {$dirname}:\n";

foreach(scandir($dirname) as $file)
{
  // Take care when constructing paths to ensure cross-platform operability
  $path = $dirname . $file;

  if (is_file($path) && isTextFile($path)) echo $path . "\n";
}

// ----------------------------

function plain_files($dirname)
{
  ($dirlist = glob($dirname . '*')) || die("Couldn't glob {$dirname}\n");

  // Pass function name directly if only a single function performs filter test
  return array_filter($dirlist, 'is_file');

  // Use, 'create_function', if a multi-function test is needed
  //
  // return array_filter($dirlist, create_function('$path', 'return is_file($path);'));
  //
}

// ------------

foreach(plain_files('/tmp/') as $path)
{
  echo $path . "\n";
}

}
?>
