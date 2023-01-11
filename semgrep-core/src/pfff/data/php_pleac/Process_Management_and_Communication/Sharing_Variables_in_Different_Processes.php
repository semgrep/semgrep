# **********************************************************************
# Sharing Variables in Different Processes
# **********************************************************************
<?php
function pleac_Sharing_Variables_in_Different_Processes() {
// sharetest - test shared variables across forks

$SHM_KEY = ftok(__FILE__, chr(1));
$handle = sem_get($SHM_KEY);
$buffer = shm_attach($handle, 1024);

// The original recipe has an INT signal handler here. However, it
// causes erratic behavior with PHP, and PHP seems to do the right
// thing without it.

for ($i = 0; $i < 10; $i++) {
    $child = pcntl_fork();
    if ($child == -1) {
        die('cannot fork');
    } elseif ($child) {
        $kids[] = $child; // in case we care about their pids
    } else {
        squabble();
        exit();
    }
}

while (true) {
    print 'Buffer is ' . shm_get_var($buffer, 1) . "\n";
    sleep(1);
}
die('Not reached');

function squabble() {
    global $handle;
    global $buffer;
    $i = 0;
    $pid = getmypid();
    while (true) {
        if (preg_match("/^$pid\\b/", shm_get_var($buffer, 1))) continue;
        sem_acquire($handle);
        $i++;
        shm_put_var($buffer, 1, "$pid $i");
        sem_release($handle);
    }
}

// Buffer is 14357 1
// Buffer is 14355 3
// Buffer is 14355 4
// Buffer is 14354 5
// Buffer is 14353 6
// Buffer is 14351 8
// Buffer is 14351 9
// Buffer is 14350 10
// Buffer is 14348 11
// Buffer is 14348 12
// Buffer is 14357 10
// Buffer is 14357 11
// Buffer is 14355 13
// ...

}
?>
