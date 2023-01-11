# **********************************************************************
# Temporarily Overriding a Signal Handler
# **********************************************************************
<?php
function pleac_Temporarily_Overriding_a_Signal_Handler() {
// the signal handler
function ding($signal) {
    fwrite(STDERR, "\x07Enter your name!\n");
}

// prompt for name, overriding SIGINT
function get_name() {
    declare(ticks = 1);
    pcntl_signal(SIGINT, 'ding');

    echo "Kindly Stranger, please enter your name: ";
    while (!@stream_select($read=array(STDIN),
                           $write=null,
                           $except=null,
                           1)) {
        // allow signals to be observed
    }
    $name = fgets(STDIN);

    // Since pcntl_signal() doesn't return the old signal handler, the
    // best we can do here is set it back to the default behavior.
    pcntl_signal(SIGINT, SIG_DFL);

    return $name;
}

}
?>
