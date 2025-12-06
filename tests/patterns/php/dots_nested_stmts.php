<?php

function foo()
{
    $var = 10;

    // MATCH:
    if ($var === 42) {
        echo('matched');
    }
    
    // MATCH:
    if ($var === 42) {
        echo('matched');
    } else {
        echo('not matched');
    }
}
