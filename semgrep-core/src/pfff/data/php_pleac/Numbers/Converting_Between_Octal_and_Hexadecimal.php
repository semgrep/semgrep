# **********************************************************************
# Converting Between Octal and Hexadecimal
# **********************************************************************
<?php
function pleac_Converting_Between_Octal_and_Hexadecimal() {
// Like C, PHP supports decimal-alternate notations. Thus, for example, the integer
// value, 867, is expressable in literal form as:
//
//   Hexadecimal -> 0x363
//   Octal       -> 01543
//
// For effecting such conversions using strings there is 'sprintf' and 'sscanf'.

$dec = 867;
$hex = sprintf('%x', $dec);
$oct = sprintf('%o', $dec);

// ------------

$dec = 0;
$hex = '363';

sscanf($hex, '%x', $dec);

// ------------

$dec = 0;
$oct = '1543';

sscanf($oct, '%o', $dec);

// ----------------------------

$number = 0;

printf('Gimme a number in decimal, octal, or hex: ');
sscanf(fgets(STDIN), '%D', $number);

printf("%d %x %o\n", $number, $number, $number);

}
?>
