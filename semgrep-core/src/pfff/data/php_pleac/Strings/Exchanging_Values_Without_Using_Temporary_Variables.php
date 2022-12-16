# **********************************************************************
# Exchanging Values Without Using Temporary Variables
# **********************************************************************
<?php
function pleac_Exchanging_Values_Without_Using_Temporary_Variables() {
#-----------------------------
list($VAR1, $VAR2) = array($VAR2, $VAR1);
#-----------------------------
$temp    = $a;
$a       = $b;
$b       = $temp;
#-----------------------------
$a       = "alpha";
$b       = "omega";
list($a, $b) = array($b, $a);        # the first shall be last -- and versa vice
#-----------------------------
list($alpha, $beta, $production) = Array("January","March","August");
# move beta       to alpha,
# move production to beta,
# move alpha      to production
list($alpha, $beta, $production) = array($beta, $production, $alpha);
#-----------------------------

}
?>
