# **********************************************************************
# Expanding Variables in User Input
# **********************************************************************
<?php
function pleac_Expanding_Variables_in_User_Input() {
#-----------------------------
$text = preg_replace('/\$(\w+)/e', '$$1', $text);
#-----------------------------
list($rows, $cols) = Array(24, 80);
$text = 'I am $rows high and $cols long';
$text = preg_replace('/\$(\w+)/e', '$$1', $text);
print $text;

#-----------------------------
$text = "I am 17 years old";
$text = preg_replace('/(\d+)/e', '2*$1', $text);
#-----------------------------
# expand variables in $text, but put an error message in
# if the variable isn't defined
$text = preg_replace('/\$(\w+)/e','isset($$1)?$$1:\'[NO VARIABLE: $$1]\'', $text);
#-----------------------------

// As PHP arrays are used as hashes too, separation of section 4
// and section 5 makes little sense.

}
?>
