# **********************************************************************
# Making a Process Look Like a File with Named Pipes
# **********************************************************************
<?php
function pleac_Making_a_Process_Look_Like_a_File_with_Named_Pipes() {
// -----------------------------
// % mkfifo /path/to/named.pipe
// -----------------------------

$fifo = fopen('/path/to/named.pipe', 'r');
if ($fifo !== false) {
    while (!feof($fifo)) {
        $line = fgets($fifo);
        if ($line === false) break;
        echo "Got: $line";
    }
    fclose($fifo);
} else {
    die('could not open fifo for read');
}

// -----------------------------

$fifo = fopen('/path/to/named.pipe', 'w');
if ($fifo !== false) {
    fwrite($fifo, "Smoke this.\n");
    fclose($fifo);
} else {
    die('could not open fifo for write');
}

// -----------------------------
// % mkfifo ~/.plan                    #  isn't this everywhere yet?
// % mknod  ~/.plan p                  #  in case you don't have mkfifo
// -----------------------------

// dateplan - place current date and time in .plan file
while (true) {
    $home = getenv('HOME');
    $fifo = fopen("$home/.plan", 'w');
    if ($fifo === false) {
        die("Couldn't open $home/.plan for writing.\n");
    }
    fwrite($fifo,
           'The current time is '
           . strftime('%a, %d %b %Y %H:%M:%S %z')
           . "\n");
    fclose($fifo);
    sleep(1);
}

// -----------------------------

// fifolog - read and record log msgs from fifo

$fifo = null;

declare(ticks = 1);
function handle_alarm($signal) {
    global $fifo;
    if ($fifo) fclose($fifo);   // move on to the next queued process
}
pcntl_signal(SIGALRM, 'handle_alarm');

while (true) {
    pcntl_alarm(0);             // turn off alarm for blocking open
    $fifo = fopen('/tmp/log', 'r');
    if ($fifo === false) {
        die("can't open /tmp/log");
    }
    pcntl_alarm(1);             // you have 1 second to log

    $service = fgets($fifo);
    if ($service === false) continue; // interrupt or nothing logged
    $service = rtrim($service);

    $message = fgets($fifo);
    if ($message === false) continue; // interrupt or nothing logged
    $message = rtrim($message);

    pcntl_alarm(0);             // turn off alarms for message processing

    if ($service == 'http') {
        // ignoring
    } elseif ($service == 'login') {
        // log to /var/log/login
        $log = fopen('/var/log/login', 'a');
        if ($log !== false) {
            fwrite($log,
                   strftime('%a, %d %b %Y %H:%M:%S %z')
                   . " $service $message\n");
            fclose($log);
        } else {
            trigger_error("Couldn't log $service $message to /var/log/login\n",
                          E_USER_WARNING);
        }
    }
}

}
?>
