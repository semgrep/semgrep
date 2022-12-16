# **********************************************************************
# Preprocessing Input
# **********************************************************************
<?php
function pleac_Preprocessing_Input() {
// Process command-line arguments using fopen(). PHP supports URLs for
// filenames as long as the "allow_url_fopen" configuration option is set.
//
// Valid URL protocols include:
//   - http://www.myserver.com/myfile.html
//   - ftp://ftp.myserver.com/myfile.txt
//   - compress.zlib://myfile.gz
//   - php://stdin
//
// See http://www.php.net/manual/en/wrappers.php for details.
//
$filenames = array_slice($argv, 1);
if (!$filenames) $filenames = array('php://stdin');
foreach ($filenames as $filename) {
    $handle = @fopen($filename, 'r');
    if ($handle) {
        while (!feof($handle)) {
            $line = fgets($handle);
            if ($line === false) break;
            // ...
        }
        fclose($handle);
    } else {
        die("can't open $filename\n");
    }
}

}
?>
