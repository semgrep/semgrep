<?php

<?php

function foo($x) {
  $x = 1;
  $y = $x;
  // Note that type inference is flow insensitive, so
  // growing the type of $x (union(int, array)) will grow the
  // type of $y too. Is it good? I think yes. An assignement
  // is a dataflow between two variables.

  // You cant use a technique a la W, or compose_subst
  // and have alpha->int, beta->alpha so beta->int
  // because later $x may grow and you want to back propagate that
  // information on all variables that dependended on it.
  // But W and compose_subst makes this hard.
  $x = array(1);
  return $y;
}
