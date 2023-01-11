# **********************************************************************
# Timing Out an Operation
# **********************************************************************
<?php
function pleac_Timing_Out_an_Operation() {
declare(ticks = 1);
$aborted = false;

function handle_alarm($signal) {
    global $aborted;
    $aborted = true;
}
pcntl_signal(SIGALRM, 'handle_alarm');

pcntl_alarm(3600);
// long-time operations here
pcntl_alarm(0);
if ($aborted) {
    // timed out - do what you will here
}
}
?>
