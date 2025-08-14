<?php

$x1 = foobar(source);
// ruleid: test
sink($x1);

$x2 = boolval(source);
// ok: test
sink($x2);

$x3 = is_bool(source);
// ok: test
sink($x3);

$x4 = source || source;
// ok:test
sink($x4);
