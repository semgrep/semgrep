# **********************************************************************
# Reversing a String by Word or Character
# **********************************************************************
<?php
function pleac_Reversing_a_String_by_Word_or_Character() {
#-----------------------------
$revchars = strrev($string);
#-----------------------------
$revwords = implode(" ", array_reverse(explode(" ", $string)));
#-----------------------------
// reverse word order
$string = 'Yoda said, "can you see this?"';
$allwords    = explode(" ", $string);
$revwords    = implode(" ", array_reverse($allwords));
print $revwords . "\n";
// this?" see you "can said, Yoda
#-----------------------------
$revwords = implode(" ", array_reverse(explode(" ", $string)));
#-----------------------------
$revwords = implode(" ", array_reverse(preg_split("/(\s+)/", $string)));
#-----------------------------
$word = "reviver";
$is_palindrome = ($word === strrev($word));
#-----------------------------
// quite a one-liner since "php" does not have a -n switch
// php -r 'while (!feof(STDIN)) { $word = rtrim(fgets(STDIN)); if ($word == strrev($word) && strlen($word) > 5) print $word; }' < /usr/dict/words
#-----------------------------

}
?>
