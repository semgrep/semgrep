<?php
function foo() {
    
    $x = 1;

    // ruleid:test
    if (bar() == Bar()) {
        $x = $x * 2;
    }

    // ruleid:test
    if ($bar() == $bar()) {
        $x = $x * 3;
    }

    // ok:test
    if (baz() == Bar()) {
        $x = $x * 5;
    }
    
    return $x;
}

echo foo();
?>
