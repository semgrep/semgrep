<?php

class A {
  static public  $var = 1;
}

$s = 'A';
echo $s::$var;
