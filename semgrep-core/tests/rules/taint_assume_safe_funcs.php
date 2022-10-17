<?php

function not_tainted($data) {
    // do stuff
    return '2';
}

//Problem here is that `tainted('a')` is in itself considered a sink because
//currently taint-mode considers anyting that falls within a sink-range to be
//a sink... and the sink range here is whatever it matches `sink(...)`!
//todook:tainted
sink(not_tainted(tainted('a')));

$ok = not_tainted(tainted('a'));
//OK:
sink($ok);

//ruleid:tainted
sink(tainted('b'));

$bad = tainted('b');
//ruleid:tainted
sink($bad);
