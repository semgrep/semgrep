# **********************************************************************
# Gathering Output from a Program
# **********************************************************************
<?php
function pleac_Gathering_Output_from_a_Program() {
// Run a command and return its results as a string.
$output_string = shell_exec('program args');

// Same as above, using backtick operator.
$output_string = `program args`;

// Run a command and return its results as a list of strings,
// one per line.
$output_lines = array();
exec('program args', $output_lines);

// -----------------------------

// The only way to execute a program without using the shell is to
// use pcntl_exec(). However, there is no way to do redirection, so
// you can't capture its output.

$pid = pcntl_fork();
if ($pid == -1) {
    die('cannot fork');
} elseif ($pid) {
    pcntl_waitpid($pid, $status);
} else {
    // Note that pcntl_exec() automatically prepends the program name
    // to the array of arguments; the program name cannot be spoofed.
    pcntl_exec($program, array($arg1, $arg2));
}

}
?>
