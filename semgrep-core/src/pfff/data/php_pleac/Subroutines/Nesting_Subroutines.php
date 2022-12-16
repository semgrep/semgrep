# **********************************************************************
# Nesting Subroutines
# **********************************************************************
<?php
function pleac_Nesting_Subroutines() {
// *** Warning *** Whilst PHP *does* allow functions to be defined within other
// functions it needs to be clearly understood that these 'inner' functions:
// * Do not exist until the outer function is called a first time, at which time
//   they then remain defined
// * Are global in scope, so are accessable outside the function by their name;
//   the fact that they are nested within another function has, AFAICT, no bearing
//   on name resolution
// * Do not form a closure: the inner function is merely 'parked' within the
//   outer function, and has no implicit access to the outer function's variables
//   or other inner functions

function outer($arg)
{
  $x = $arg + 35;
  function inner() { return $x * 19; }

  // *** wrong *** 'inner' returns 0 * 19, not ($arg + 35) * 19
  return $x + inner();
}

// ----------------------------

function outer($arg)
{
  $x = $arg + 35;

  // No implicit access to outer function scope; any required data must be
  // explicity passed
  function inner($x) { return $x * 19; }

  return $x + inner($x);
}

// ------------

// Equivalent to previously-shown code
function inner($x)
{
  return $x * 19;
}

function outer($arg)
{
  $x = $arg + 35;
  return $x + inner($x);
}

}
?>
