# **********************************************************************
# Reading STDERR from a Program
# **********************************************************************
<?php
function pleac_Reading_STDERR_from_a_Program() {
$output = `cmd 2>&1`;                          // with backticks
// or
$ph = popen('cmd 2>&1');                       // with an open pipe
while (!feof($ph)) { $line = fgets($ph); }     // plus a read
// -----------------------------
$output = `cmd 2>/dev/null`;                   // with backticks
// or
$ph = popen('cmd 2>/dev/null');                // with an open pipe
while (!feof($ph)) { $line = fgets($ph); }     // plus a read
// -----------------------------
$output = `cmd 2>&1 1>/dev/null`;              // with backticks
// or
$ph = popen('cmd 2>&1 1>/dev/null');           // with an open pipe
while (!feof($ph)) { $line = fgets($ph); }     // plus a read
// -----------------------------
$output = `cmd 3>&1 1>&2 2>&3 3>&-`;           // with backticks
// or
$ph = popen('cmd 3>&1 1>&2 2>&3 3>&-|');       // with an open pipe
while (!feof($ph)) { $line = fgets($ph); }     // plus a read
// -----------------------------
exec('program args 1>/tmp/program.stdout 2>/tmp/program.stderr');
// -----------------------------
$output = `cmd 3>&1 1>&2 2>&3 3>&-`;
// -----------------------------
$fd3 = $fd1;
$fd1 = $fd2;
$fd2 = $fd3;
$fd3 = null;
// -----------------------------
exec('prog args 1>tmpfile 2>&1');
exec('prog args 2>&1 1>tmpfile');
// -----------------------------
// exec('prog args 1>tmpfile 2>&1');
$fd1 = "tmpfile";        // change stdout destination first
$fd2 = $fd1;             // now point stderr there, too
// -----------------------------
// exec('prog args 2>&1 1>tmpfile');
$fd2 = $fd1;             // stderr same destination as stdout
$fd1 = "tmpfile";        // but change stdout destination

}
?>
