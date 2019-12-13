<?php

1;
foo(1); // I'm a clown
2;
foo(
  1);
3;
foo(1); /* I'm a 
really bad clown */
4;
// this should be kept
foo(1); // this should be removed
// this should be kept too
