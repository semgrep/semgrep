<?php

$x1 = foobar(source);
// ruleid: test
sink($x1);

$x2 = intval(source);
// ok: test
sink($x2);

$x3 = floatval(source);
// ok: test
sink($x3);

$x4 = source + source;
// ok:test
sink($x4);

$x5 = source - source;
// ok:test
sink($x5);

$x6 = source * source;
// ok:test
sink($x6);

$x7 = source / source;
// ok:test
sink($x7);

$x8 = source % source;
// ok:test
sink($x8);
