# **********************************************************************
# Interpolating Functions and Expressions Within Strings
# **********************************************************************
<?php
function pleac_Interpolating_Functions_and_Expressions_Within_Strings() {
#-----------------------------
echo $var1 . func() . $var2; // scalar only
#-----------------------------
// PHP can only handle variable expression without operators
// $answer = "STRING ${[ VAR EXPR ]} MORE STRING";
#-----------------------------
$phrase = "I have " . ($n + 1) . " guanacos.";
// PHP cannot handle the complex exression: ${\($n + 1)}
#-----------------------------
// Rest of Discussion is not applicable to PHP
#-----------------------------
// Interpolating functions not available in PHP
#-----------------------------

}
?>
