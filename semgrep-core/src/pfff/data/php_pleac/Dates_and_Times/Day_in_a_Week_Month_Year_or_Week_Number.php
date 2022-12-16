# **********************************************************************
# Day in a Week/Month/Year or Week Number
# **********************************************************************
<?php
function pleac_Day_in_a_Week_Month_Year_or_Week_Number() {
// 'getdate' accepts a timestamp [or implicitly calls 'time'] and returns an array of
// date components. It returns much the same information as 'strptime' except that
// the component names are different

$today = getdate();

$weekday = $today['wday'];
$monthday = $today['mday'];
$yearday = $today['yday'];

$weeknumber = (int) round($yearday / 7.0);

// Safter method of obtaining week number
$weeknumber = strftime('%U') + 1;

// ----------------------------

define(SEP, '/');

$day = 16;
$month = 6;
$year = 1981;

$timestamp = mktime(0, 0, 0, $month, $day, $year);

$date = getdate($timestamp);

$weekday = $date['wday'];
$monthday = $date['mday'];
$yearday = $date['yday'];

$weeknumber = (int) round($yearday / 7.0);

$weeknumber = strftime('%U', $timestamp) + 1;

// Interpolate ...
$sep = SEP;
echo "{$month}{$sep}{$day}{$sep}{$year} was a {$date['weekday']} in week {$weeknumber}\n";

// ... or, concatenate
echo $month . SEP . $day . SEP . $year . ' was a ' . $date['weekday']
     . ' in week ' . $weeknumber . "\n";

}
?>
