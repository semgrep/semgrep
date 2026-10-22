<?php

function test1($cond)
{
  if ($cond)
    $x = "tainted";
  else
    $x = "safe";
  //ruleid: taint-maturity
  sink($x);
}

function test2($cond)
{
  $x = "tainted";
  if ($cond)
    $x = sanitize($x);
  //ruleid: taint-maturity
  sink($x);
  $x = sanitize($x);
  //OK: taint-maturity
  sink($x);
}

function test3($cond)
{
  $x = "tainted";
  if ($cond)
  {
    //ruleid: taint-maturity
    sink($x);
    $y = sanitize($x);
  }
  else
    $y = sanitize($x);
  //ruleid: taint-maturity
  sink($x);
  //OK: taint-maturity
  sink($y);
}

