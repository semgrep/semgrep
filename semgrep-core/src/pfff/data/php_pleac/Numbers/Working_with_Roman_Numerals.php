# **********************************************************************
# Working with Roman Numerals
# **********************************************************************
<?php
function pleac_Working_with_Roman_Numerals() {
// PHP offers no native support for Roman Numerals. However, a 'Numbers_Roman' class
// is available for download from PEAR: [http://pear.php.net/package/Numbers_Roman].
// Note the following 'include' directives are required:
//
//   include_once('Numbers/Roman.php');

$roman = Numbers_Roman::toNumeral($arabic);
$arabic = Numbers_Roman::toNumber($roman);

// ----------------------------

$roman_fifteen = Numbers_Roman::toNumeral(15);

$arabic_fifteen = Numbers_Roman::toNumber($roman_fifteen);

printf("Roman for fifteen is: %s\n", $roman_fifteen);
printf("Arabic for fifteen is: %d\n", $arabic_fifteen);

}
?>
