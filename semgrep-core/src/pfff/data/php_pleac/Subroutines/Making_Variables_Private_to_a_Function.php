# **********************************************************************
# Making Variables Private to a Function
# **********************************************************************
<?php
function pleac_Making_Variables_Private_to_a_Function() {
// Strictly-speaking, PHP is neither lexically [no environment capture] nor
// dynamically [no variable shadowing] scoped. A script in which several
// functions have been defined has two, entirely separate, scopes:
//
// * A 'top-level' scope i.e. everything outside each function
//
// * A 'local scope' within each function; each function is a self-contained
//   entity and cannot [via conventional means] access variables outside its
//   local scope. Accessing a variable that has not been locally defined
//   serves to define it i.e. accessing a variable assumed to be global
//   sees a local variable of that name defined
//
// The way 'global' variables are provided is via a predefined array of
// variable names, $GLOBALS [it is one of a special set of variables known
// as 'superglobals'; such variables *are* accessable in all scopes]. Each
// entry in this array is a 'global' variable name, and may be freely
// accessed / updated. A more convenient means of accessing such variables
// is via the 'global' keyword: one or more variables within a function is
// declared 'global', and those names are then taken to refer to entries
// in the $GLOBALS array rather than seeing local variables of that name
// accessed or defined

function some_func()
{
  // Variables declared within a function are local to that function
  $variable = 'something';
}

// ----------------------------

// Top-level declared variables
$name = $argv[1]; $age = $argv[2];

$c = fetch_time();

$condition = 0;

// ------------

function run_check()
{
  // The globally-declared variable, '$condition', is not accessable within
  // the function unless it declared as 'global. Had this not been done then
  // attempts to access, '$condition', would have seen a local variable
  // of that name declared and updated. Same applies to other variables
  global $condition, $name, $age, $c;

  $condition = 1;
  // ...
}

function check_x($x)
{
  $y = 'whatever';

  // This function only has access to the parameter, '$x', and the locally
  // declared variable, '$y'.

  // Whilst 'run_check' has access to several global variables, the current
  // function does not. For it to access the global variable, '$condition',
  // it must be declared 'global'
  run_check();

  global $condition;

  // 'run_check' will have updated, '$condition', and since it has been
  // declared 'global' here, it is accessable

  if ($condition)
  {
    ; // ...
  }
}

}
?>
