# **********************************************************************
# Getting and Setting Timestamps
# **********************************************************************
<?php
function pleac_Getting_and_Setting_Timestamps() {
$filename = 'example.txt';

// Get the file's current access and modification time, respectively
$fs = stat($filename);

$readtime = $fs['atime'];
$writetime = $fs['mtime'];

// Alter $writetime, and $readtime ...

// Update file timestamp
touch($filename, $writetime, $readtime);

// ----------------------------

$filename = 'example.txt';

// Get the file's current access and modification time, respectively
$fs = stat($filename);

$atime = $fs['atime'];
$mtime = $fs['mtime'];

// Dedicated functions also exist to retrieve this information:
//
// $atime = $fileatime($filename);
// $mtime = $filemtime($filename);
//

// Perform date arithmetic. Traditional approach where arithmetic is performed
// directly with Epoch Seconds [i.e. the *NIX time stamp value] will work ...

define('SECONDS_PER_DAY', 60 * 60 * 24);

// Set file's access and modification times to 1 week ago
$atime -= 7 * SECONDS_PER_DAY;
$mtime -= 7 * SECONDS_PER_DAY;

// ... but care must be taken to account for daylight saving. Therefore, the
// recommended approach is to use library functions to perform such tasks:

$atime = strtotime('-7 days', $atime);
$mtime = strtotime('-7 days', $mtime);

// Update file timestamp
touch($filename, $mtime, $atime);

// Good idea to clear the cache after such updates have occurred so fresh
// values will be retrieved on next access
clearstatcache();

// ----------------------------

$argc == 2 || die("usage: {$argv[0]} filename\n");

$filename = $argv[1];
$fs = stat($filename);

$atime = $fs['atime'];
$mtime = $fs['mtime'];

// Careful here: since interactive, use, 'system', not 'exec', to launch [latter
// does not work under *NIX - at least, not for me :)]
system(trim(getenv('EDITOR') . ' vi ' . escapeshellarg($filename)), $retcode);

touch($filename, $mtime, $atime) || die("Error updating timestamp on file, {$filename}!\n");

}
?>
