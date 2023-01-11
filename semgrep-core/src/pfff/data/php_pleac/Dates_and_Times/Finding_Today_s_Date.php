# **********************************************************************
# Finding Today's Date
# **********************************************************************
<?php
function pleac_Finding_Today_s_Date() {
define(SEP, '-');

// ------------

$today = getdate();

$day = $today['mday'];
$month = $today['mon'];
$year = $today['year'];

// Either do this to use interpolation:
$sep = SEP;
echo "Current date is: {$year}{$sep}{$month}{$sep}{$day}\n";

// or simply concatenate:
echo 'Current date is: ' . $year . SEP . $month . SEP . $day . "\n";

// ------------

$today = localtime(time(), TRUE);

$day = $today['tm_mday'];
$month = $today['tm_mon'] + 1;
$year = $today['tm_year'] + 1900;

printf("Current date is: %4d%s%2d%s%2d\n", $year, SEP, $month, SEP, $day);

// ------------

$format = 'Y' . SEP . 'n' . SEP . 'd';

$today = date($format);

echo "Current date is: {$today}\n";

// ------------

$sep = SEP;

$today = strftime("%Y$sep%m$sep%d");

echo "Current date is: {$today}\n";

}
?>
