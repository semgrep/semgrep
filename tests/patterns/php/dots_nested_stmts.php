<?php

function foo()
{
    $var = 10;

    //ERROR: match
    if ($var === 42) {
        echo('matched');
    }

    if ($var === 42) {
        echo('matched');
    } else {
        echo('not matched');
    }
}
