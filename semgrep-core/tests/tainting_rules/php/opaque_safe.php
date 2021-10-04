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

//ERROR:
sink(tainted('b'));

$bad = tainted('b');
//ERROR:
sink($bad);
