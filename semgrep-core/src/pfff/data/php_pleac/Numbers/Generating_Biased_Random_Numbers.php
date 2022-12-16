# **********************************************************************
# Generating Biased Random Numbers
# **********************************************************************
<?php
function pleac_Generating_Biased_Random_Numbers() {
function random() { return (float) rand() / (float) getrandmax(); }

function gaussian_rand()
{
  $u1 = 0.0; $u2 = 0.0; $g1 = 0.0; $g2 = 0.0; $w = 0.0;

  do
  {
    $u1 = 2.0 * random() - 1.0; $u2 = 2.0 * random() - 1.0;
    $w = $u1 * $u1 + $u2 * $u2;
  } while ($w > 1.0);

  $w = sqrt((-2.0 * log($w)) / $w); $g2 = $u1 * $w; $g1 = $u2 * $w;

  return $g1;
}

// ------------

$mean = 25.0; $sdev = 2.0;
$salary = gaussian_rand() * $mean + $sdev;

printf("You have been hired at: %.2f\n", $salary);

}
?>
