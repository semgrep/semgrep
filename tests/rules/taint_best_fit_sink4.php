<?php

// ok:taint-regression-1.6.0
sink(isset($source) ? 'something' : 'b');

$a = (isset($source) ? 'something' : 'c');
// ok:taint-regression-1.6.0
sink($a);

if (isset($source)) {
    $b = 'sonething';
} else {
    $b = 'd';
}
// ok:taint-regression-1.6.0
sink($b);

// ok:taint-regression-1.6.0
sink(isset($source) ? sanitizer($source) : '');

$c = isset($source) ? sanitizer($source) : '';
// ok:taint-regression-1.6.0
sink($c);

// ruleid:taint-regression-1.6.0
sink(isset($source) ? $source : '');

// ok:taint-regression-1.6.0
sink(isset($source) ? 'aa' . $source : 'bb');

// ruleid:taint-regression-1.6.0
sink(isset($source) ? $source : 'bb');
