# **********************************************************************
# Removing a Directory and Its Contents
# **********************************************************************
<?php
function pleac_Removing_a_Directory_and_Its_Contents() {
// AFAICT, there is currently no library function that recursively removes a
// directory tree [i.e. a directory, it's subdirectories, and any other files]
// with a single call. Such a function needs to be custom built. PHP tools
// with which to do this:
// * 'unlink', 'rmdir', 'is_dir', and 'is_file' functions, will all take care
//   of the file testing and deletion
// * Actual directory traversal requires obtaining directory / subdirectory
//   lists, and here there is much choice available, though care must be taken
//   as each has it's own quirks
//   - 'opendir', 'readdir', 'closedir'
//   - 'scandir'
//   - 'glob'
//   - SPL 'directory iterator' classes [newish / experimental - not shown here]
//
// The PHP documentation for 'rmdir' contains several examples, each illustrating
// one of each approach; the example shown here is loosely based on one of these
// examples

// Recursor - recursively traverses directory tree
function rmtree_($dir)
{
  $dir = "$dir";

  if ($dh = opendir($dir))
  {
    while (FALSE !== ($item = readdir($dh)))
    {
      if ($item != '.' && $item != '..')
      {
        $subdir = $dir . '/' . "$item";

        if (is_dir($subdir)) rmtree_($subdir);
        else @unlink($subdir);
      }
    }

    closedir($dh); @rmdir($dir);
  }
}

// Launcher - performs validation then starts recursive routine
function rmtree($dir)
{
  if (is_dir($dir))
  {
    (substr($dir, -1, 1) == '/') && ($dir = substr($dir, 0, -1));
    rmtree_($dir); return !is_dir($dir);
  }

  return FALSE;
}

// ------------

$argc == 2 || die("usage: rmtree dir\n");

rmtree($argv[1]) || die("Could not remove directory {$argv[1]}\n");

}
?>
