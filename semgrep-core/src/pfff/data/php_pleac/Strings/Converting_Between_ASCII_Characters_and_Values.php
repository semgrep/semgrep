# **********************************************************************
# Converting Between ASCII Characters and Values
# **********************************************************************
<?php
function pleac_Converting_Between_ASCII_Characters_and_Values() {
#-----------------------------
$num  = ord($char);
$char = chr($num);
#-----------------------------
$char = sprintf("%c", $num);                # slower than chr($num)
printf("Number %d is character %c\n", $num, $num);
#-----------------------------
$ASCII = unpack("C*", $string);
eval('$STRING = pack("C*", '.implode(',',$ASCII).');');
#-----------------------------
$ascii_value = ord("e");    # now 101
$character   = chr(101);    # now "e"
#-----------------------------
printf("Number %d is character %c\n", 101, 101);
#-----------------------------
$ascii_character_numbers = unpack("C*", "sample");
print explode(" ",$ascii_character_numbers)."\n";

eval('$word = pack("C*", '.implode(',',$ascii_character_numbers).');');
$word = pack("C*", 115, 97, 109, 112, 108, 101);   # same
print "$word\n";
#-----------------------------
$hal = "HAL";
$ascii = unpack("C*", $hal);
foreach ($ascii as $val) {
    $val++;                 # add one to each ASCII value
}
eval('$ibm = pack("C*", '.implode(',',$ascii).');');
print "$ibm\n";             # prints "IBM"
#-----------------------------

}
?>
