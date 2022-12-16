# **********************************************************************
# Short Sleeps
# **********************************************************************
<?php
function pleac_Short_Sleeps() {
// Low-resolution: sleep time specified in seconds
sleep(1);

// High-resolution: sleep time specified in microseconds [not reliable under Windows]
usleep(250000);

}
?>
