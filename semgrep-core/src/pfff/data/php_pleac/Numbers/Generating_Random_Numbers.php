# **********************************************************************
# Generating Random Numbers
# **********************************************************************
<?php
function pleac_Generating_Random_Numbers() {
// Techniques used here simply mirror Perl examples, and are not an endorsement
// of any particular RNG technique

// In PHP do this ...
$random = rand($lowerbound, $upperbound);
$random = rand($x, $y);

// ----------------------------

function make_password($chars, $reqlen)
{
  $len = strlen($chars);
  for ($i = 0; $i < $reqlen; $i++) $password .= substr($chars, rand(0, $len), 1);
  return $password;
}

$chars = 'ABCDEfghijKLMNOpqrstUVWXYz'; $reqlen = 8;

$password = make_password($chars, $reqlen);

}
?>
