# **********************************************************************
# Calculating More Trigonometric Functions
# **********************************************************************
<?php
function pleac_Calculating_More_Trigonometric_Functions() {
function my_tan($theta) { return sin($theta) / cos($theta); }

// ------------

$theta = 3.7;

printf("%f\n", my_tan($theta));
printf("%f\n", tan($theta));

}
?>
