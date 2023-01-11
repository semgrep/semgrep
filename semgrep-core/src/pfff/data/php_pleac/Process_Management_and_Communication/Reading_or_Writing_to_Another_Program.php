# **********************************************************************
# Reading or Writing to Another Program
# **********************************************************************
<?php
function pleac_Reading_or_Writing_to_Another_Program() {
// Handle each line in the output of a process.
$readme = popen('program arguments', 'r');
while (!feof($readme)) {
    $line = fgets($readme);
    if ($line === false) break;
    // ...
}
pclose($readme);

// -----------------------------

// Write to the input of a process.
$writeme = popen('program arguments', 'w');
fwrite($writeme, 'data');
pclose($writeme);

// -----------------------------

// Wait for a process to complete.
$f = popen('sleep 1000000', 'r');  // child goes to sleep
pclose($f);                        // and parent goes to lala land

// -----------------------------

$writeme = popen('program arguments', 'w');
fwrite($writeme, "hello\n");  // program will get hello\n on STDIN
pclose($writeme);             // program will get EOF on STDIN

// -----------------------------

// Output buffering callback that sends output to the pager.
function ob_pager($output, $mode) {
    static $pipe;
    if ($mode & PHP_OUTPUT_HANDLER_START) {
        $pager = getenv('PAGER');
        if (!$pager) $pager = '/usr/bin/less';  // XXX: might not exist
        $pipe = popen($pager, 'w');
    }
    fwrite($pipe, $output);
    if ($mode & PHP_OUTPUT_HANDLER_END) {
        pclose($pipe);
    }
}

// Redirect standard output to the pager.
ob_start('ob_pager');

// Do something useful that writes to standard output, then
// close the output buffer.
// ...
ob_end_flush();

}
?>
