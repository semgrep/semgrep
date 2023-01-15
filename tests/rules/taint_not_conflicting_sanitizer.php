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

//ruleid:tainted
sink(tainted('b'));

$bad = tainted('b');
//ruleid:tainted
sink($bad);
