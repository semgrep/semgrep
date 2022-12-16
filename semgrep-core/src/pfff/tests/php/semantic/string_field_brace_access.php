<?php

class A {
}

$o = new A();

$o->fld[1] = 42;
$o->fld{2} = 43;
var_dump($o);
