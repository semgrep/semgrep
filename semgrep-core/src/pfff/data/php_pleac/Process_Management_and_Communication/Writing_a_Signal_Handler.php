# **********************************************************************
# Writing a Signal Handler
# **********************************************************************
<?php
function pleac_Writing_a_Signal_Handler() {
function got_int($signal) {
    pcntl_signal(SIGINT, 'got_int');  // but not for SIGCHLD!
    // ...
}
pcntl_signal(SIGINT, 'got_int');

// -----------------------------

declare(ticks = 1);
$interrupted = false;

function got_int($signal) {
    global $interrupted;
    $interrupted = true;
    // The third argument to pcntl_signal() determines if system calls
    // should be restarted after a signal. It defaults to true.
    pcntl_signal(SIGINT, 'got_int', false);  // or SIG_IGN
}
pcntl_signal(SIGINT, 'got_int', false);

// ... long-running code that you don't want to restart

if ($interrupted) {
    // deal with the signal
}

}
?>
