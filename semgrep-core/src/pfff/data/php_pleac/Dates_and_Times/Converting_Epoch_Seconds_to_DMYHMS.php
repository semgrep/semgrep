# **********************************************************************
# Converting Epoch Seconds to DMYHMS
# **********************************************************************
<?php
function pleac_Converting_Epoch_Seconds_to_DMYHMS() {
$dmyhms = getdate();            // timestamp: current date / time

$dmyhms = getdate($timestamp);  // timestamp: arbitrary

$day = $dmyhms['mday'];
$month = $dmyhms['mon'];
$year = $dmyhms['year'];

$hours = $dmyhms['hours'];
$minutes = $dmyhms['minutes'];
$seconds = $dmyhms['seconds'];

}
?>
