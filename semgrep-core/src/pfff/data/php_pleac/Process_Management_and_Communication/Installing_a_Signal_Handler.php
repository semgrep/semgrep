# **********************************************************************
# Installing a Signal Handler
# **********************************************************************
<?php
function pleac_Installing_a_Signal_Handler() {
// call got_sig_quit for every SIGQUIT
pcntl_signal(SIGQUIT, 'got_sig_quit');
// call got_sig_pipe for every SIGPIPE
pcntl_signal(SIGPIPE, 'got_sig_pipe');
// increment ouch for every SIGINT
function got_sig_int($signal) { global $ouch; $ouch++; }
pcntl_signal(SIGINT, 'got_sig_int');
// ignore the signal INT
pcntl_signal(SIGINT, SIG_IGN);
// restore default STOP signal handling
pcntl_signal(SIGSTOP, SIG_DFL);

}
?>
