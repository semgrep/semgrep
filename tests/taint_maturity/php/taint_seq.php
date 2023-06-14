<?php

function test1()
{
  //ruleid: taint-maturity
  sink("tainted");
}

function test2()
{
  $x = "safe";
  $a = $x;
  $x = "tainted";
  $y = $x;
  $z = $y;
  //ruleid: taint-maturity
  sink($z);
  //OK: taint-maturity
  sink($a);
  //OK: taint-maturity
  safe($z);
}

function test3()
{
  //OK: taint-maturity
  sink(sanitize("tainted"));
}

function test4()
{
  $x = "tainted";
  $x = sanitize($x);
  //OK: taint-maturity
  sink($x);
}
