# **********************************************************************
# Copying or Moving a File
# **********************************************************************
<?php
function pleac_Copying_or_Moving_a_File() {
$oldfile = '/tmp/old'; $newfile = '/tmp/new';

copy($oldfile, $newfile) || die("Error copying file\n");

// ----------------------------

// All the following copy a file by copying its contents. Examples do so in a single
// operation, but it is also possible to copy arbitrary blocks, or, line-by-line in
// the case of 'text' files
$oldfile = '/tmp/old'; $newfile = '/tmp/new';

if (is_file($oldfile))
  file_put_contents($newfile, file_get_contents($oldfile));
else
  die("Problem copying file {$oldfile} to file {$newfile}\n");

// ------------

$oldfile = '/tmp/old'; $newfile = '/tmp/new';

fwrite(($nh = fopen($newfile, 'wb')), fread(($oh = fopen($oldfile, 'rb')), filesize($oldfile)));
fclose($oh);
fclose($nh);

// ------------

// As above, but with some error checking / handling
$oldfile = '/tmp/old'; $newfile = '/tmp/new';

($oh = fopen($oldfile, 'rb')) || die("Problem opening input file {$oldfile}\n");
($nh = fopen($newfile, 'wb')) || die("Problem opening output file {$newfile}\n");

if (($filesize = filesize($oldfile)) > 0)
{
  fwrite($nh, fread($oh, $filesize)) || die("Problem reading / writing file data\n");
}

fclose($oh);
fclose($nh);

// ----------------------------

// Should there be platform-specfic problems copying 'very large' files, it is
// a simple matter to call a system command utility via, 'exec'

// *NIX-specific example. Could check whether, 'exec', succeeded, but checking whether
// a file exists after the operation might be a better approach
$oldfile = '/tmp/old'; $newfile = '/tmp/new';

is_file($newfile) && unlink($newfile);

exec(trim('cp --force ' . escapeshellarg($oldfile) . ' ' . escapeshellarg($newfile)));

is_file($newfile) || die("Problem copying file {$oldfile} to file {$newfile}\n");

// For other operating systems just change:
// * filenames
// * command being 'exec'ed
// as the rest of the code is platform independant

}
?>
