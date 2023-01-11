# **********************************************************************
# Generating Different Random Numbers
# **********************************************************************
<?php
function pleac_Generating_Different_Random_Numbers() {
// PHP sports a large number of C Standard Library routines including the 'srand'
// function, used to re-seed the RNG used with calls to the 'rand' function. Thus,
// as per Perl example:

while (TRUE)
{
  $seed = (int) fgets(STDIN);
  if (!empty($seed)) break;
}

srand($seed);

}
?>
