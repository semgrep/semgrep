# **********************************************************************
# Globbing, or Getting a List of Filenames Matching a Pattern
# **********************************************************************
<?php
function pleac_Globbing__or_Getting_a_List_of_Filenames_Matching_a_Pattern() {
$dirname = '/tmp/';

// Full paths
$pathlist = glob($dirname . '*.c');

// File names only - glob-based matching
$filelist = array_filter(scandir($dirname),
                         create_function('$file', 'return fnmatch("*.c", $file);'));

// ----------------------------

$dirname = '/tmp/';

// File names only - regex-based matching [case-insensitive]
$filelist = array_filter(scandir($dirname),
                         create_function('$file', 'return eregi("\.[ch]$", $file);'));

// ----------------------------

$dirname = '/tmp/';

// Directory names - all-digit names
$dirs = array_filter(glob($dirname . '*', GLOB_ONLYDIR),
                     create_function('$path', 'return ereg("^[0-9]+$", basename($path));'));

}
?>
