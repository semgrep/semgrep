<?php

$foo1 = new BAR($x, $y, $z);
# ruleid: test
$foo1->exec($args);

$foo2 = new ALICE($x, $y, $z);
# ok
$foo2->exec($args);
