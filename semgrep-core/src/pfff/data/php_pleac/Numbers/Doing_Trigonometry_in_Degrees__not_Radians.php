# **********************************************************************
# Doing Trigonometry in Degrees, not Radians
# **********************************************************************
<?php
function pleac_Doing_Trigonometry_in_Degrees__not_Radians() {
// 'deg2rad' and 'rad2deg' are actually PHP built-ins, but here is how you might implement
//  them if needed
function deg2rad_($deg) { return ($deg / 180.0) * M_PI; }
function rad2deg_($rad) { return ($rad / M_PI) * 180.0; }

// ------------

printf("%f\n", deg2rad_(180.0));
printf("%f\n", deg2rad(180.0));

// ----------------------------

function degree_sin($deg) { return sin(deg2rad($deg)); }

// ------------

$rad = deg2rad(380.0);

printf("%f\n", sin($rad));
printf("%f\n", degree_sin(380.0));

}
?>
