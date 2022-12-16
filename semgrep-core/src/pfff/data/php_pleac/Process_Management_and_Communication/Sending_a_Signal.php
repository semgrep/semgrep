# **********************************************************************
# Sending a Signal
# **********************************************************************
<?php
function pleac_Sending_a_Signal() {
// send pid a signal 9
posix_kill($pid, 9);
// send whole job a signal 1
posix_kill($pgrp, -1);
// send myself a SIGUSR1
posix_kill(getmypid(), SIGUSR1);
// send a SIGHUP to processes in pids
foreach ($pids as $pid) posix_kill($pid, SIGHUP);

// -----------------------------

// Use kill with pseudo-signal 0 to see if process is alive.
if (posix_kill($minion, 0)) {
    echo "$minion is alive!\n";
} else {
    echo "$minion is deceased.\n";
}

}
?>
