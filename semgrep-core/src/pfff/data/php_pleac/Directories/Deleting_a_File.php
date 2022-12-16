# **********************************************************************
# Deleting a File
# **********************************************************************
<?php
function pleac_Deleting_a_File() {
// The 'unlink' function is used to delete regular files, whilst the 'rmdir' function
// does the same on non-empty directories. AFAIK, no recursive-deletion facility
// exists, and must be manually programmed

$filename = '...';

@unlink($filename) || die("Can't delete, {$filename}!\n");

// ------------

$files = glob('...');
$problem = FALSE;

// Could simply use a foreach loop
foreach($files as $filename) { @unlink($filename) || $problem = TRUE; }

//
// Alternatively, an applicative approach could be used, one closer in spirit to
// largely-functional languages like Scheme
//
// function is_all_deleted($deleted, $filename) { return @unlink($filename) && $deleted; }
// $problem = !array_reduce($files, 'is_all_deleted', TRUE);
//

if ($problem)
{
  fwrite(STDERR, 'Could not delete all of:');
  foreach($files as $filename) { fwrite(STDERR, ' ' . $filename); }
  fwrite(STDERR, "\n"); exit(1);
}

// ------------

function rmAll($files)
{
  $count = 0;

  foreach($files as $filename) { @unlink($filename) && $count++; };

  return $count;

// An applicative alternative using 'create_function', PHP's rough equivalent of 'lambda' ...
//
//  return array_reduce($files,
//    create_function('$count, $filename', 'return @unlink($filename) && $count++;'), 0);
}

// ----

$files = glob('...');
$toBeDeleted = sizeof($files);
$count = rmAll($files);

($count == $toBeDeleted) || die("Could only delete {$count} of {$toBeDeleted} files\n");

}
?>
