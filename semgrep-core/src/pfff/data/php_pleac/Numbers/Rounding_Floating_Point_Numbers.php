# **********************************************************************
# Rounding Floating-Point Numbers
# **********************************************************************
<?php
function pleac_Rounding_Floating_Point_Numbers() {
// Preferred approach
$rounded = round($unrounded, $precision);

// Possible alternate approach
$format = '%[width].[prec]f';
$rounded = sprintf($format, $unrounded);

// ------------

$a = 0.255; $b = round($a, 2);
echo "Unrounded: {$a}\nRounded: {$b}\n";

$a = 0.255; $b = sprintf('%.2f', $a);
echo "Unrounded: {$a}\nRounded: {$b}\n";

$a = 0.255;
printf("Unrounded: %.f\nRounded: %.2f\n", $a, $a);

// ----------------------------

echo "number\tint\tfloor\tceil\n";

foreach(array(3.3, 3.5, 3.7, -3.3) as $number)
{
  printf("%.1f\t%.1f\t%.1f\t%.1f\n", $number, (int) $number, floor($number), ceil($number));
}

}
?>
