# **********************************************************************
# Introduction
# **********************************************************************
<?php
function pleac_Introduction() {
#-----------------------------
$string = '\n';                     # two characters, \ and an n
$string = 'Jon \'Maddog\' Orwant';  # literal single quotes
$string = 'Jon "Maddog" Orwant';    # literal double quotes
#-----------------------------
$string = "\n";                     # a "newline" character
$string = "Jon \"Maddog\" Orwant";  # literal double quotes
$string = "Jon 'Maddog' Orwant";    # literal single quotes
#-----------------------------
$a =
"This is a multiline
here document";

$a = <<<EOF
This is a multiline here document
terminated by EOF on a line by itself
EOF;
#-----------------------------

}
?>
