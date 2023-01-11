# **********************************************************************
# Multiplying Matrices
# **********************************************************************
<?php
function pleac_Multiplying_Matrices() {
// PHP offers no native support for matrices. However, a 'Math_Matrix' class
// is available for download from PEAR: [http://pear.php.net/package/Math_Matrix].
// Note the following 'include' directives are required:
//
//  include_once('Math/Matrix.php');

$a = new Math_Matrix(array(array(3, 2, 3), array(5, 9, 8)));
$b = new Math_Matrix(array(array(4, 7), array(9, 3), array(8, 1)));

echo $a->toString() . "\n";
echo $b->toString() . "\n";

// NOTE: When I installed this package I had to rename the 'clone' method else
// it would not load, so I chose to rename it to 'clone_', and this usage is
// shown below. This bug may well be fixed by the time you obtain this package

$c = $a->clone_();
$c->multiply($b);

echo $c->toString() . "\n";

}
?>
