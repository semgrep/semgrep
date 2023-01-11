# **********************************************************************
# Processing a String One Character at a Time
# **********************************************************************
<?php
function pleac_Processing_a_String_One_Character_at_a_Time() {
#-----------------------------
// using perl regexp
$array = preg_split('//', $string ,-1, PREG_SPLIT_NO_EMPTY);
// using PHP function: $array = str_split($string);

// Cannot use unpack with a format of 'U*' in PHP.
#-----------------------------
for ($offset = 0; preg_match('/(.)/', $string, $matches, 0, $offset) > 0; $offset++) {
    // $matches[1] has charcter, ord($matches[1]) its number
}
#-----------------------------
$seen = array();
$string = "an apple a day";
foreach (str_split($string) as $char) {
    $seen[$char] = 1;
}
$keys = array_keys($seen);
sort($keys);
print "unique chars are: " . implode('', $keys) . "\n";
// unique chars are:  adelnpy
#-----------------------------
$seen = array();
$string = "an apple a day";
for ($offset = 0; preg_match('/(.)/', $string, $matches, 0, $offset) > 0; $offset++) {
    $seen[$matches[1]] = 1;
}
$keys = array_keys($seen);
sort($keys);
print "unique chars are: " . implode('', $keys) . "\n";
// unique chars are:  adelnpy
#-----------------------------
$sum = 0;
foreach (unpack("C*", $string) as $byteval) {
    $sum += $byteval;
}
print "sum is $sum\n";
// prints "1248" if $string was "an apple a day"
#-----------------------------
$sum = array_sum(unpack("C*", $string));
#-----------------------------

// sum - compute 16-bit checksum of all input files
$handle = @fopen($argv[1], 'r');
$checksum = 0;
while (!feof($handle)) {
    $checksum += (array_sum(unpack("C*", fgets($handle))));
}
$checksum %= pow(2,16) - 1;
print "$checksum\n";

# @@INCLUDE@@ include/php/slowcat.php
#-----------------------------

}
?>
