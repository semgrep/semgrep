<?php

const glob1 = "password";

$glob2 = "password";
$glob2 = "bar";

function foo()
{
    // ERROR: match
    bar(glob1);

    bar($glob2);
}
