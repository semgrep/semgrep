# **********************************************************************
# Adding to or Subtracting from a Date
# **********************************************************************
<?php
function pleac_Adding_to_or_Subtracting_from_a_Date() {
// Date arithmetic is probably most easily performed using timestamps [i.e. *NIX Epoch
// Seconds]. Dates - in whatever form - are converted to timestamps, these are
// arithmetically manipulated, and the result converted to whatever form required.
// Note: use 'mktime' to create timestamps properly adjusted for daylight saving; whilst
// 'strtotime' is more convenient to use, it does not, AFAIK, include this adjustment

$when = $now + $difference;
$then = $now - $difference;

// ------------

$now = mktime(0, 0, 0, 8, 6, 2003);

$diff1 = dateOffset('day=1'); $diff2 = dateOffset('weeks=2');

echo 'Today is:                 ' . date('Y-m-d', $now) . "\n";
echo 'One day in the future is: ' . date('Y-m-d', $now + $diff1) . "\n";
echo 'Two weeks in the past is: ' . date('Y-m-d', $now - $diff2) . "\n";

// ----------------------------

// Date arithmetic performed using a custom function, 'dateOffset'. Internally, offset may
// be computed in one of several ways:
// * Direct timestamp manipulation - fastest, but no daylight saving adjustment
// * Via 'date' built-in function - slower [?], needs a base time from which to
//   compute values, but has daylight saving adjustment
// * Via 'strtotime' built-in function - as for 'date'
// * Via 'DateTime' class
//
// Approach used here is to utilise direct timestamp manipulation in 'dateOffset' [it's
// performance can also be improved by replacing $tbl with a global definition etc],
// and to illustrate how the other approaches might be used

// 1. 'dateOffset'

$birthtime = mktime(3, 45, 50, 1, 18, 1973);

$interval = dateOffset('day=55', 'hours=2', 'min=17', 'sec=5');

$then = $birthtime + $interval;

printf("Birthtime is: %s\nthen is:      %s\n", date(DATE_RFC1123, $birthtime), date(DATE_RFC1123, $then));

// ------------

// 2. 'date'

// Base values, and offsets, respectively
$hr = 3; $min = 45; $sec = 50; $mon = 1; $day = 18; $year = 1973;

$yroff = 0; $monoff = 0; $dayoff = 55; $hroff = 2; $minoff = 17; $secoff = 5;

// Base date
$birthtime = mktime($hr, $min, $sec, $mon, $day, $year, TRUE);

$year = date('Y', $birthtime) + $yroff;
$mon = date('m', $birthtime) + $monoff;
$day = date('d', $birthtime) + $dayoff;

$hr = date('H', $birthtime) + $hroff;
$min = date('i', $birthtime) + $minoff;
$sec = date('s', $birthtime) + $secoff;

// Offset date
$then = mktime($hr, $min, $sec, $mon, $day, $year, TRUE);

printf("Birthtime is: %s\nthen is:      %s\n", date(DATE_RFC1123, $birthtime), date(DATE_RFC1123, $then));

// ------------

// 3. 'strtotime'

// Generate timestamp whatever way is preferable
$birthtime = mktime(3, 45, 50, 1, 18, 1973);
$birthtime = strtotime('1/18/1973 03:45:50');

$then = strtotime('+55 days 2 hours 17 minutes 2 seconds', $birthtime);

printf("Birthtime is: %s\nthen is:      %s\n", date(DATE_RFC1123, $birthtime), date(DATE_RFC1123, $then));

// ------------

// 4. 'DateTime' class

$birthtime = new DateTime('1/18/1973 03:45:50');
$then = new DateTime('1/18/1973 03:45:50');
$then->modify('+55 days 2 hours 17 minutes 2 seconds');

printf("Birthtime is: %s\nthen is:      %s\n", $birthtime->format(DATE_RFC1123), $then->format(DATE_RFC1123));

}
?>
