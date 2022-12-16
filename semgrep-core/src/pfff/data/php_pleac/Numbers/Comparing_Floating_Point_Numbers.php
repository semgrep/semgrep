# **********************************************************************
# Comparing Floating-Point Numbers
# **********************************************************************
<?php
function pleac_Comparing_Floating_Point_Numbers() {
// In PHP floating point comparisions are 'safe' [meaning the '==' comparison operator
// can be used] as long as the value consists of 14 digits or less [total digits, either
// side of the decimal point e.g. xxxxxxx.xxxxxxx, xxxxxxxxxxxxxx., .xxxxxxxxxxxxxx]. If
// values with more digits must be compared, then:
//
// * Represent as strings, and take care to avoid any implicit conversions e.g. don't pass
//   a float as a float to a function and expect all digits to be retained - they won't be -
//   then use 'strcmp' to compare the strings
//
// * Avoid float use; perhaps use arbitrary precision arithmetic. In this case, the
//   'bccomp' function is relevant

// Will work as long as each floating point value is 14 digits or less
if ($float_1 == $float_2)
{
  ; // ...
}

// Compare as strings
$cmp = strcmp('123456789.123456789123456789', '123456789.123456789123456788');

// Use 'bccomp'
$precision = 5; // Number of significant comparison digits after decimal point
if (bccomp('1.111117', '1.111116', $precision))
{
  ; // ...
}

$precision = 6;
if (bccomp('1.111117', '1.111116', $precision))
{
  ; // ...
}

// ----------------------------

$wage = 536;
$week = $wage * 40;
printf("One week's wage is: $%.2f\n", $week / 100);

}
?>
