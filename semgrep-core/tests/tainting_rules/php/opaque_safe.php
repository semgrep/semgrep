<?php

function not_tainted($data) {
    // do stuff
    return '2';
}
x = not_tainted(tainted('a'));
// ok:tainted
sink(x);
