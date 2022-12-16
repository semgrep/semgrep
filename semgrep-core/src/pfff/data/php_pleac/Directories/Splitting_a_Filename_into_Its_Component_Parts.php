# **********************************************************************
# Splitting a Filename into Its Component Parts
# **********************************************************************
<?php
function pleac_Splitting_a_Filename_into_Its_Component_Parts() {
$base = basename($path);
$dir = dirname($path);

// PHP's equivalent to Perl's 'fileparse'
$pathinfo = pathinfo($path);

$base = $pathinfo['basename'];
$dir = $pathinfo['dirname'];
$ext = $pathinfo['extension'];

// ----------------------------

$path = '/usr/lib/libc.a';

printf("dir is %s, file is %s\n", dirname($path), basename($path));

// ------------

$path = '/usr/lib/libc.a';

$pathinfo = pathinfo($path);

printf("dir is %s, name is %s, extension is %s\n", $pathinfo['dirname'], $pathinfo['basename'], $pathinfo['extension']);

// ----------------------------

// Handle Mac example as a simple parse task. However, AFAIK, 'pathinfo' is cross-platform,
// so should handle file path format differences transparently
$path = 'Hard%20Drive:System%20Folder:README.txt';

$macp = array_combine(array('drive', 'folder', 'filename'), split("\:", str_replace('%20', ' ', $path)));
$macf = array_combine(array('name', 'extension'), split("\.", $macp['filename']));

printf("dir is %s, name is %s, extension is %s\n", ($macp['drive'] . ':' . $macp['folder']), $macf['name'], ('.' . $macf['extension']));

// ----------------------------

// Not really necessary since we have, 'pathinfo', but better matches Perl example
function file_extension($filename, $separator = '.')
{
  return end(split(("\\" . $separator), $filename));
}

// ----

echo file_extension('readme.txt') . "\n";

}
?>
