# **********************************************************************
# Soundex Matching
# **********************************************************************
<?php
function pleac_Soundex_Matching() {
#-----------------------------
$code = soundex($string);
#-----------------------------
$phoned_words = metaphone("Schwern");
#-----------------------------
// substitution function for getpwent():
// returns an array of user entries,
// each entry contains the username and the full name
function getpwent() {
    $pwents = array();
    $handle = fopen("passwd", "r");
    while (!feof($handle)) {
        $line = fgets($handle);
        if (preg_match("/^#/", $line)) continue;
        $cols = explode(":", $line);
        $pwents[$cols[0]] = $cols[4];
    }
    return $pwents;
}

print "Lookup user: ";
$user = rtrim(fgets(STDIN));
if (empty($user)) exit;
$name_code = soundex($user);
$pwents = getpwent();
foreach($pwents as $username => $fullname) {
    preg_match("/(\w+)[^,]*\b(\w+)/", $fullname, $matches);
    list(, $firstname, $lastname) = $matches;

    if ($name_code == soundex($username) ||
        $name_code == soundex($lastname) ||
        $name_code == soundex($firstname))
    {
        printf("%s: %s %s\n", $username, $firstname, $lastname);
    }
}
#-----------------------------

}
?>
