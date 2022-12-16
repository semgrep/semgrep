# **********************************************************************
# Escaping Characters
# **********************************************************************
<?php
function pleac_Escaping_Characters() {
#-----------------------------
//backslash
$var = preg_replace('/([CHARLIST])/', '\\\$1', $var);
// double
$var = preg_replace('/([CHARLIST])/', '$1$1', $var);
#-----------------------------
$var = preg_replace('/%/', '%%', $var);
#-----------------------------
$string = 'Mom said, "Don\'t do that."';
$string = preg_replace('/([\'"])/', '\\\$1', $string);
// in PHP you can also use the addslashes() function
#-----------------------------
$string = 'Mom said, "Don\'t do that."';
$string = preg_replace('/([\'"])/', '$1$1', $string);
#-----------------------------
$string = preg_replace('/([^A-Z])/', '\\\$1', $string);
#-----------------------------
// PHP does not have the \Q and \E string metacharacters
$string = "this is\\ a\\ test\\!";
// PHP's quotemeta() function is not the same as perl's quotemeta() function
$string = preg_replace('/(\W)/', '\\\$1', 'is a test!');
#-----------------------------

}
?>
