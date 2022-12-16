# **********************************************************************
# Checking Whether a String Is a Valid Number
# **********************************************************************
<?php
function pleac_Checking_Whether_a_String_Is_a_Valid_Number() {
// Two basic approaches to numeric validation:
// * Built-in functions like 'is_numeric', 'is_int', 'is_float' etc
// * Regexes, as shown below

$s = '12.345';

preg_match('/\D/', $s) && die("has nondigits\n");
preg_match('/^\d+$/', $s) || die("not a natural number\n");
preg_match('/^-?\d+$/', $s) || die("not an integer\n");
preg_match('/^[+-]?\d+$/', $s) || die("not an integer\n");
preg_match('/^-?\d+\.?\d*$/', $s) || die("not a decimal\n");
preg_match('/^-?(?:\d+(?:\.\d*)?|\.\d+)$/', $s) || die("not a decimal\n");
preg_match('/^([+-]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))?$/', $s) || die("not a C float\n");

// ----------------------------

function getnum($s)
{
  sscanf($s, "%D", $number); return isset($number) ? $number : 0;
}

echo getnum(123) . "\n";   // ok
echo getnum(0xff) . "\n";  // ..
echo getnum(044) . "\n";   // ..

echo getnum('x') . "\n";   // fail

}
?>
