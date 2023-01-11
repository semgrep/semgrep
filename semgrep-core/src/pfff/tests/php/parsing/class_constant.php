<?php

class A {
  const TEST = 1;
}

interface I {
 // facebook-ext:
//  abstract const ABS_TEST;
  const NORM_TEST = 1;
}

class B implements I {
  const ABS_TEST = 1;
}

abstract class C implements I {}

var_dump(A::TEST);
var_dump(B::ABS);
