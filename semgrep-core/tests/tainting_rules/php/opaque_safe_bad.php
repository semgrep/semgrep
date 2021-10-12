<?php

function not_tainted($data) {
    // do stuff
    return '2';
}

//OK:
sink(not_tainted(tainted('a')));

$ok = not_tainted(tainted('a'));
//OK:
sink($ok);

// $F(...) sanitizes *everything*
//OK:
sink(tainted('b'));

$bad = tainted('b');
// $F(...) sanitizes *everyting*
//OK:
sink($bad);
