# **********************************************************************
# Accessing Subroutine Arguments
# **********************************************************************
<?php
function pleac_Accessing_Subroutine_Arguments() {
// Conventionally-defined function together with parameter list
function hypotenuse($side1, $side2)
{
  return sqrt(pow($side1, 2) + pow($side2, 2));
}

// ----

// Alternative is to define the function without parameter list, then use
// 'func_get_arg' to extract arguments
function hypotenuse()
{
  // Could check number of arguments passed with: 'func_num_args', which
  // would be the approach used if dealing with variable number of arguments
  $side1 = func_get_arg(0); $side2 = func_get_arg(1);

  return sqrt(pow($side1, 2) + pow($side2, 2));
}

// ------------

// 1. Conventional function call
$diag = hypotenuse(3, 4);

// ------------

// 2. Function call using, 'call_user_func' library routines
$funcname = 'hypotenuse';

// a. Pass function name, and variable number of arguments
$diag = call_user_func($funcname, 3, 4);

// b. Package arguments as array, pass together with function name
$args = array(3, 4);
$diag = call_user_func_array($funcname, $args);

// ----------------------------

$nums = array(1.4, 3.5, 6.7);

// ------------

// Pass-by-value
function int_all($arr)
{
  return array_map(create_function('$n', 'return (int) $n;'), $arr);
}

// Pass-by-reference
function trunc_em(&$n)
{
  foreach ($n as &$value) $value = (int) $value;
}

// ------------

// $nums untouched; $ints is new array
$ints = int_all($nums);

// $nums updated
trunc_em($nums);

}
?>
