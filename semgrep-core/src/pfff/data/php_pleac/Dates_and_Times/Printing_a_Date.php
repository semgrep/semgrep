# **********************************************************************
# Printing a Date
# **********************************************************************
<?php
function pleac_Printing_a_Date() {
// 'date' and 'strftime' both print a date string based on:
// * Format String, describing layout of date components
// * Timestamp [*NIX Epoch Seconds], either given explicitly, or implictly
//   via a call to 'time' which retrieves current time value

$ts = 1234567890;

date('Y/m/d', $ts);
date('Y/m/d', mktime($h, $m, $s, $mth, $d, $y, $is_dst));

date('Y/m/d');         // same as: date('Y/m/d', time());

// ------------

$ts = 1234567890;

strftime('%Y/%m/%d', $ts);
strftime('%Y/%m/%d', mktime($h, $m, $s, $mth, $d, $y, $is_dst));

strftime('%Y/%m/%d');  // same as: strftime('%Y/%m/%d', time());

// ----------------------------

// 'mktime' creates a local time timestamp
$t = strftime('%a %b %e %H:%M:%S %z %Y', mktime(3, 45, 50, 1, 18, 73, TRUE));
echo "{$t}\n";

// 'gmmktime' creates a GMT time timestamp
$t = strftime('%a %b %e %H:%M:%S %z %Y', gmmktime(3, 45, 50, 1, 18, 73));
echo "{$t}\n";

// ----------------------------

// 'strtotime' parses a textual date expression, and generates a timestamp
$t = strftime('%A %D', strtotime('18 Jan 1973, 3:45:50'));
echo "{$t}\n";

// This should generate output identical to previous example
$t = strftime('%A %D', mktime(3, 45, 50, 1, 18, 73, TRUE));
echo "{$t}\n";

}
?>
