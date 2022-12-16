# **********************************************************************
# Taking Logarithms
# **********************************************************************
<?php
function pleac_Taking_Logarithms() {
$value = 100.0;

$log_e = log($value);
$log_10 = log10($value);

// ----------------------------

function log_base($base, $value) { return log($value) / log($base); }

// ------------

$answer = log_base(10.0, 10000.0);

printf("log(10, 10,000) = %f\n", $answer);

}
?>
