<?php
// closures inheriting variables from the parent scope
$baz = 'pew pew pew';
$foo = function( $bar ) use ( $baz ) {
};
