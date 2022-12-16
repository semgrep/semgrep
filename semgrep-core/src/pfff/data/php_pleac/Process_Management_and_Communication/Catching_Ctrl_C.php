# **********************************************************************
# Catching Ctrl-C
# **********************************************************************
<?php
function pleac_Catching_Ctrl_C() {
// ignore signal INT
pcntl_signal(SIGINT, SIG_IGN);

// install signal handler
declare(ticks = 1);
function tsktsk($signal) {
    fwrite(STDERR, "\x07The long habit of living indisposeth us for dying.");
    pcntl_signal(SIGINT, 'tsktsk');
}
pcntl_signal(SIGINT, 'tsktsk');

}
?>
