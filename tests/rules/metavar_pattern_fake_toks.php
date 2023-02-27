<?php

// ruleid:regression-1.9.0
if ( ! something( $a ) ) {
    echo 'failed';
}

// ok:regression-1.9.0
if ( ! something( $a ) ) {
    die('ok');
}

// ok:regression-1.9.0
if ( ! something( $aa ) ) {
    $s = 3;
    die('ok');
}

// ruleid:regression-1.9.0
if ( isset( $b ) && something( $b ) ) {
    echo 'failed';
}

// ok:regression-1.9.0
if ( isset( $b ) && something( $b ) ) {
    die('ok');
}

// ok:regression-1.9.0
if ( isset( $b ) && something( $b ) ) {
    $s = 5;
    die('ok');
}

// ok:regression-1.9.0
if ( something( $c ) ) {
    break;
}

// ok:regression-1.9.0
if ( something( $cc ) ) {
    $s = 3;
    break;
}


