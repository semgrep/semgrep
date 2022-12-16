<?php

$x = array();
$x[] = 1; //

$x[][] = 1; //
$x[][ 'foo' ] = 'bar'; //

$y = & $x[]; //
list( $x[], $y ) = explode( '@', 'foo@bar' ); //
