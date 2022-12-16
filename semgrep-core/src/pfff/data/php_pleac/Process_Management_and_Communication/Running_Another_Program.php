# **********************************************************************
# Running Another Program
# **********************************************************************
<?php
function pleac_Running_Another_Program() {
// Run a simple command and retrieve its result code.
exec("vi $myfile", $output, $result_code);

// -----------------------------

// Use the shell to perform redirection.
exec('cmd1 args | cmd2 | cmd3 >outfile');
exec('cmd args <infile >outfile 2>errfile');

// -----------------------------

// Run a command, handling its result code or signal.
$pid = pcntl_fork();
if ($pid == -1) {
    die('cannot fork');
} elseif ($pid) {
    pcntl_waitpid($pid, $status);
    if (pcntl_wifexited($status)) {
        $status = pcntl_wexitstatus($status);
        echo "program exited with status $status\n";
    } elseif (pcntl_wifsignaled($status)) {
        $signal = pcntl_wtermsig($status);
        echo "program killed by signal $signal\n";
    } elseif (pcntl_wifstopped($status)) {
        $signal = pcntl_wstopsig($status);
        echo "program stopped by signal $signal\n";
    }
} else {
    pcntl_exec($program, $args);
}

// -----------------------------

// Run a command while blocking interrupt signals.
$pid = pcntl_fork();
if ($pid == -1) {
    die('cannot fork');
} elseif ($pid) {
    // parent catches INT and berates user
    declare(ticks = 1);
    function handle_sigint($signal) {
        echo "Tsk tsk, no process interruptus\n";
    }
    pcntl_signal(SIGINT, 'handle_sigint');
    while (!pcntl_waitpid($pid, $status, WNOHANG)) {}
} else {
    // child ignores INT and does its thing
    pcntl_signal(SIGINT, SIG_IGN);
    pcntl_exec('/bin/sleep', array('10'));
}

// -----------------------------

// Since there is no direct access to execv() and friends, and
// pcntl_exec() won't let us supply an alternate program name
// in the argument list, there is no way to run a command with
// a different name in the process table.

}
?>
