<?php

// see http://php.net/manual/en/language.types.string.php
// and the "variable parsing" section

$beer = 'foo';
// "simple syntax" according to doc
echo "X $beer X\n";
echo "X {$beer}s X\n";
// "complex syntax" according to doc
echo "X ${beer} X\n";
