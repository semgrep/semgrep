<?php

$var = source();
// ruleid:regression_0116
sink($var);

if ( $var  == 'aaa' || $var  == 'bb' ) {
    $aa = 'bb';
}

// ruleid:regression_0116
sink('aaa' . $var);

if ($var == 'a') {
    // ok:regression_0116
    sink($var);
}
