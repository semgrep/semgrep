# **********************************************************************
# Avoiding Zombie Processes
# **********************************************************************
<?php
function pleac_Avoiding_Zombie_Processes() {
pcntl_signal(SIGCHLD, SIG_IGN);

// -----------------------------

declare(ticks = 1);
function reaper($signal) {
    $pid = pcntl_waitpid(-1, $status, WNOHANG);
    if ($pid > 0) {
        // ...
        reaper($signal);
    }
    // install *after* calling waitpid
    pcntl_signal(SIGCHLD, 'reaper');
}
pcntl_signal(SIGCHLD, 'reaper');

// -----------------------------

declare(ticks = 1);
function reaper($signal) {
    $pid = pcntl_waitpid(-1, $status, WNOHANG);
    if ($pid == -1) {
        // No child waiting. Ignore it.
    } else {
        if (pcntl_wifexited($signal)) {
            echo "Process $pid exited.\n";
        } else {
            echo "False alarm on $pid\n";
        }
        reaper($signal);
    }
    pcntl_signal(SIGCHLD, 'reaper');
}
pcntl_signal(SIGCHLD, 'reaper');

}
?>
