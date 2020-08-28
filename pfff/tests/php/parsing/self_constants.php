<?php

//IMHO this should be forbidden because "self"/"SELF" should be considered
// as keywords, but PHP allows it and people have used it.

class A {
  const SELF = 1;
}

echo A::SELF;

// although echo A::self; will generate a parse error
