<?php
use My\Full\Classname as Test;
// ERROR: match
$obj = new My\Full\Classname();
// ERROR: match
$obj = new Test();
$obj = new Classname();

use Foo\Bar\Classname;
$obj = new Classname();

use Foo\Bar\A as Classname;
$obj = new Classname();

use My\Full\A as Classname;
$obj = new Classname();
