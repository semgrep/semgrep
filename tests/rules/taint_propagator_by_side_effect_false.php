<?php

// ruleid:match
echo $_GET['a'];
// ok:match
echo 'nop';

// ruleid:match
echo foo($_GET['a']);

// ok:match
echo foo('a');

// ruleid:match
print_r($_GET);

// ruleid:match
print_r($_GET, false);

// ruleid:match
echo print_r($_GET, true);

// ok:match
print_r($_GET, true);

// ok:match
print_r('a');

// ok:match
printf('a');

hook('action', function() {
    // ruleid:match
    echo $_GET['b'];

    // ruleid:match
    echo foo($_GET['b']);

    // ok:match
    echo foo('a');

    // ok:match
    echo bar($_GET['b']);

    // ok:match
    echo 'nop';
});
