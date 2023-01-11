# **********************************************************************
# Using Complex Numbers
# **********************************************************************
<?php
function pleac_Using_Complex_Numbers() {
// PHP offers no native support for complex numbers. However, a 'Math_Complex' class
// is available for download from PEAR: [http://pear.php.net/package/Math_Complex].
// Note the following 'include' directives are required:
//
//   include_once('Math/Complex.php');
//   include_once('Math/TrigOp.php');
//   include_once('Math/ComplexOp.php');

$a = new Math_Complex(3, 5);
$b = new Math_Complex(2, -2);

$c = Math_ComplexOp::mult($a, $b);

echo $c->toString() . "\n";

// ----------------------------

$d = new Math_Complex(3, 4);
$r = Math_ComplexOp::sqrt($d);

echo $r->toString() . "\n";

}
?>
