# **********************************************************************
# Controlling Case
# **********************************************************************
<?php
function pleac_Controlling_Case() {
#-----------------------------
$big = strtoupper($little);
$little = strtolower($big);
// PHP does not have the\L and\U string escapes.
#-----------------------------
$big = ucfirst($little);
$little = strtolower(substr($big, 0, 1)) . substr($big, 1);
#-----------------------------
$beast   = "dromedary";
// capitalize various parts of $beast
$capit   = ucfirst($beast); // Dromedar
// PHP does not have the\L and\U string escapes.
$capall  = strtoupper($beast); // DROMEDAR
// PHP does not have the\L and\U string escapes.
$caprest = strtolower(substr($beast, 0, 1)) . substr(strtoupper($beast), 1); // dROMEDAR
// PHP does not have the\L and\U string escapes.
#-----------------------------
// titlecase each word's first character, lowercase the rest
$text = "thIS is a loNG liNE";
$text = ucwords(strtolower($text));
print $text;
// This Is A Long Line
#-----------------------------
if (strtoupper($a) == strtoupper($b)) { // or strcasecmp($a, $b) == 0
    print "a and b are the same\n";
}
#-----------------------------
# @@INCLUDE@@ include/php/randcap.php

// % php randcap.php < genesis | head -9
#-----------------------------

}
?>
