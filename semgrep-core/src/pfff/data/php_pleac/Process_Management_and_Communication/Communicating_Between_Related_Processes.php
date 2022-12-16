# **********************************************************************
# Communicating Between Related Processes
# **********************************************************************
<?php
function pleac_Communicating_Between_Related_Processes() {
// PHP supports fork/exec/wait but not pipe. However, it does
// support socketpair, which can do everything pipes can as well
// as bidirectional communication. The original recipes have been
// modified here to use socketpair only.

// -----------------------------

// pipe1 - use socketpair and fork so parent can send to child
$sockets = array();
if (!socket_create_pair(AF_UNIX, SOCK_STREAM, 0, $sockets)) {
    die(socket_strerror(socket_last_error()));
}
list($reader, $writer) = $sockets;

$pid = pcntl_fork();
if ($pid == -1) {
    die('cannot fork');
} elseif ($pid) {
    socket_close($reader);
    $line = sprintf("Parent Pid %d is sending this\n", getmypid());
    if (!socket_write($writer, $line, strlen($line))) {
        socket_close($writer);
        die(socket_strerror(socket_last_error()));
    }
    socket_close($writer);
    pcntl_waitpid($pid, $status);
} else {
    socket_close($writer);
    $line = socket_read($reader, 1024, PHP_NORMAL_READ);
    printf("Child Pid %d just read this: `%s'\n", getmypid(), rtrim($line));
    socket_close($reader);  // this will happen anyway
    exit(0);
}

// -----------------------------

// pipe2 - use socketpair and fork so child can send to parent
$sockets = array();
if (!socket_create_pair(AF_UNIX, SOCK_STREAM, 0, $sockets)) {
    die(socket_strerror(socket_last_error()));
}
list($reader, $writer) = $sockets;

$pid = pcntl_fork();
if ($pid == -1) {
    die('cannot fork');
} elseif ($pid) {
    socket_close($writer);
    $line = socket_read($reader, 1024, PHP_NORMAL_READ);
    printf("Parent Pid %d just read this: `%s'\n", getmypid(), rtrim($line));
    socket_close($reader);
    pcntl_waitpid($pid, $status);
} else {
    socket_close($reader);
    $line = sprintf("Child Pid %d is sending this\n", getmypid());
    if (!socket_write($writer, $line, strlen($line))) {
        socket_close($writer);
        die(socket_strerror(socket_last_error()));
    }
    socket_close($writer);  // this will happen anyway
    exit(0);
}

// -----------------------------

// pipe3 and pipe4 demonstrate the use of perl's "forking open"
// feature to reimplement pipe1 and pipe2. pipe5 uses two pipes
// to simulate socketpair. Since PHP supports socketpair but not
// pipe, and does not have a "forking open" feature, these
// examples are skipped here.

// -----------------------------

// pipe6 - bidirectional communication using socketpair
$sockets = array();
if (!socket_create_pair(AF_UNIX, SOCK_STREAM, 0, $sockets)) {
    die(socket_strerror(socket_last_error()));
}
list($child, $parent) = $sockets;

$pid = pcntl_fork();
if ($pid == -1) {
    die('cannot fork');
} elseif ($pid) {
    socket_close($parent);
    $line = sprintf("Parent Pid %d is sending this\n", getmypid());
    if (!socket_write($child, $line, strlen($line))) {
        socket_close($child);
        die(socket_strerror(socket_last_error()));
    }
    $line = socket_read($child, 1024, PHP_NORMAL_READ);
    printf("Parent Pid %d just read this: `%s'\n", getmypid(), rtrim($line));
    socket_close($child);
    pcntl_waitpid($pid, $status);
} else {
    socket_close($child);
    $line = socket_read($parent, 1024, PHP_NORMAL_READ);
    printf("Child Pid %d just read this: `%s'\n", getmypid(), rtrim($line));
    $line = sprintf("Child Pid %d is sending this\n", getmypid());
    if (!socket_write($parent, $line, strlen($line))) {
        socket_close($parent);
        die(socket_strerror(socket_last_error()));
    }
    socket_close($parent);
    exit(0);
}

}
?>
