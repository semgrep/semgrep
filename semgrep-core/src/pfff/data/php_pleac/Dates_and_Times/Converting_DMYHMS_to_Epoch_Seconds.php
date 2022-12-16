# **********************************************************************
# Converting DMYHMS to Epoch Seconds
# **********************************************************************
<?php
function pleac_Converting_DMYHMS_to_Epoch_Seconds() {
$timestamp = mktime($hour, $min, $sec, $month, $day, $year);

$timestamp = gmmktime($hour, $min, $sec, $month, $day, $year);

}
?>
