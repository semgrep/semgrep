<?php

$a = 0;
unset($a);
//error at runtime with HPHP
$a++;
echo $a;
