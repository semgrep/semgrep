# **********************************************************************
# Trimming Blanks from the Ends of a String
# **********************************************************************
<?php
function pleac_Trimming_Blanks_from_the_Ends_of_a_String() {
#-----------------------------
$string = trim($string);
#-----------------------------
// print what's typed, but surrounded by > < symbols
while (!feof(STDIN)) {
    print ">" . substr(fgets(STDIN), 0, -1) . "<\n";
}
#-----------------------------
$string = preg_replace('/\s+/', ' ', $string); // finally, collapse middle
#-----------------------------
$string = trim($string);
$string = preg_replace('/\s+/', ' ', $string);
#-----------------------------
// 1. trim leading and trailing white space
// 2. collapse internal whitespace to single space each
function sub_trim($string) {
    $string = trim($string);
    $string = preg_replace('/\s+/', ' ', $string);
    return $string;
}
#-----------------------------

}
?>
