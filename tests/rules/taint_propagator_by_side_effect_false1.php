<?php

// ruleid:match
echo $_GET['a'];
// ok:match
echo 'nop';

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
