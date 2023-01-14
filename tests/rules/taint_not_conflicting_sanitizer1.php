<?php

function not_tainted($data) {
    // do stuff
    return '2';
}

//OK:tainted
sink(not_tainted(tainted('a')));

$ok = not_tainted(tainted('a'));
//OK:tainted
sink($ok);

// $F(...) sanitizes *everything*
//OK:tainted
sink(tainted('b'));

$bad = tainted('b');
// $F(...) sanitizes *everyting*
//OK:tainted
sink($bad);
