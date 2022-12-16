# **********************************************************************
# Redefining a Function
# **********************************************************************
<?php
function pleac_Redefining_a_Function() {
// In PHP once a function has been defined it remains defined. In other words,
// it cannot be undefined / deleted, nor can that particular function name be
// reused to reference another function body. Even the lambda-like functions
// created via the 'create_function' built-in, cannot be undefined [they exist
// until script termination, thus creating too many of these can actually
// exhaust memory !]. However, since the latter can be assigned to variables,
// the same variable name can be used to reference difference functions [and
// when this is done the reference to the previous function is lost (unless
// deliberately saved), though the function itself continues to exist].
//
// If, however, all that is needed is a simple function aliasing facility,
// then just assign the function name to a variable, and execute using the
// variable name

// Original function
function expand() { echo "expand\n"; }

// Prove that function exists
echo (function_exists('expand') ? 'yes' : 'no') . "\n";

// Use a variable to alias it
$grow = 'expand';

// Call function via original name, and variable, respectively
expand();

$grow();

// Remove alias variable
unset($grow);

// ----------------------------

function fred() { echo "fred\n"; }

$barney = 'fred';

$barney();

unset($barney);

fred();

// ------------

$fred = create_function('', 'echo "fred\n";');

$barney = $fred;

$barney();

unset($barney);

$fred();

// ----------------------------

function red($text) { return "<FONT COLOR='red'>$text</FONT>"; }

echo red('careful here') . "\n";

// ------------

$colour = 'red';

$$colour = create_function('$text', 'global $colour;
return "<FONT COLOR=\'$colour\'>$text</FONT>";');

echo $$colour('careful here') . "\n";

unset($$colour);

// ----

$colours = split(' ', 'red blue green yellow orange purple violet');

foreach ($colours as $colour)
{
  $$colour = create_function('$text', 'global $colour;
  return "<FONT COLOR=\'$colour\'>$text</FONT>";');
}

foreach ($colours as $colour) { echo $$colour("Careful with this $colour, James") . "\n"; }

foreach ($colours as $colour) { unset($$colour); }

}
?>
