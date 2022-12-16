# **********************************************************************
# Difference of Two Dates
# **********************************************************************
<?php
function pleac_Difference_of_Two_Dates() {
// Date intervals are most easily computed using timestamps [i.e. *NIX Epoch
// Seconds] which, of course, gives the interval result is seconds from which
// all other interval measures [days, weeks, months, years] may be derived.
// Refer to previous section for discussion of daylight saving and other related
// problems

$interval_seconds = $recent - $earlier;

// ----------------------------

// Conventional approach ...
$bree = strtotime('16 Jun 1981, 4:35:25');
$nat = strtotime('18 Jan 1973, 3:45:50');

// ... or, with daylight saving adjustment
$bree = mktime(4, 35, 25, 6, 16, 1981, TRUE);
$nat = mktime(3, 45, 50, 1, 18, 1973, TRUE);

$difference = $bree - $nat;

// 'dateInterval' custom function computes intervals in several measures given an
// interval in seconds. Note, 'month' and 'year' measures not provided
printf("There were %d seconds between Nat and Bree\n", $difference);
printf("There were %d weeks between Nat and Bree\n", dateInterval('weeks', $difference));
printf("There were %d days between Nat and Bree\n", dateInterval('days', $difference));
printf("There were %d hours between Nat and Bree\n", dateInterval('hours', $difference));
printf("There were %d minutes between Nat and Bree\n", dateInterval('mins', $difference));

}
?>
