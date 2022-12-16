# **********************************************************************
# Accessing Substrings
# **********************************************************************
<?php
function pleac_Accessing_Substrings() {
#-----------------------------
$value = substr($string, $offset, $count);
$value = substr($string, $offset);

$string = substr_replace($string, $newstring, $offset, $count);
$string = substr_replace($string, $newtail, $offset);
#-----------------------------
# get a 5-byte string, skip 3, then grab 2 8-byte strings, then the rest
list($leading, $s1, $s2, $trailing) =
  array_values(unpack("A5a/x3/A8b/A8c/A*d", $data));

# split at five byte boundaries
preg_match_all ("/.{5}/", $data, $f, PREG_PATTERN_ORDER);
$fivers = $f[0];

# chop string into individual characters
$chars  = $string;
#-----------------------------
$string = "This is what you have";
#         +012345678901234567890  Indexing forwards  (left to right)
#          109876543210987654321- Indexing backwards (right to left)
#           note that 0 means 10 or 20, etc. above

$first  = substr($string, 0, 1);  # "T"
$start  = substr($string, 5, 2);  # "is"
$rest   = substr($string, 13);    # "you have"
$last   = substr($string, -1);    # "e"
$end    = substr($string, -4);    # "have"
$piece  = substr($string, -8, 3); # "you"
#-----------------------------
$string = "This is what you have";
print $string;
#This is what you have

$string = substr_replace($string, "wasn't", 5, 2);  # change "is" to "wasn't"
#This wasn't what you have

$string = substr_replace($string, "ondrous", -12);  # "This wasn't wondrous"
#This wasn't wondrous

$string = substr_replace($string, "", 0, 1);        # delete first character
#his wasn't wondrous

$string = substr_replace($string, "", -10);         # delete last 10 characters
#his wasn'
#-----------------------------
if (preg_match("/pattern/", substr($string, -10))) {
    print "Pattern matches in last 10 characters\n";
}

# substitute "at" for "is", restricted to first five characters
$string=(substr_replace(preg_replace("/is/", "at", substr($string,0,5)),0,5));
#-----------------------------
# exchange the first and last letters in a string
$a = "make a hat";
list($a[0], $a[strlen($a)-1]) = Array(substr($a,-1), substr($a,0,1));
print $a;

#-----------------------------
# extract column with unpack
$a = "To be or not to be";
$b = unpack("x6/A6a", $a);  # skip 6, grab 6
print $b['a'];


$b = unpack("x6/A2b/X5/A2c", $a); # forward 6, grab 2; backward 5, grab 2
print $b['b']."\n".$b['c']."\n";

#-----------------------------
function cut2fmt() {
    $positions = func_get_args();
    $template  = '';
    $lastpos   = 1;
    foreach($positions as $place) {
        $template .= "A" . ($place - $lastpos) . " ";
        $lastpos   = $place;
    }
    $template .= "A*";
    return $template;
}

$fmt = cut2fmt(8, 14, 20, 26, 30);
print "$fmt\n";
#A7 A6 A6 A6 A4 A*
#-----------------------------

}
?>
