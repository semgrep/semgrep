# **********************************************************************
# Trapping Undefined Function Calls with AUTOLOAD
# **********************************************************************
<?php
function pleac_Trapping_Undefined_Function_Calls_with_AUTOLOAD() {
// PHP sports an AUTOLOAD facility that is quite easy to use, but, AFAICT, is geared
// towards the detection of unavailable classes rather than for individual functions.
// Here is a rudimentary example:

function __autoload($classname)
{
  if (!file_exists($classname))
  {
    // Class file does not exist, so handle situation; in this case,
    // issue error message, and exit program
    die("File for class: {$classname} not found - aborting\n");
  }
  else
  {
    // Class file exists, so load it
    require_once $classname;
  }
}

// ------------

// Attempt to instantiate object of undefined class
new UnknownClassObject();

// Execution continues here if class exists
// ...

// ----------------------------

// It is also possible to perform [quite extensive] introspection on functions,
// variables etc, so it is possible to check whether a function exists before
// executing it, thus allowing a non-existent functions to be searched for and
// loaded from a source file, or perhaps dynamically defined. An example of what
// could be described as a custom autoload facility appears below.

$colours = array('red', 'blue', 'green', 'yellow', 'orange', 'purple', 'violet');

foreach ($colours as $colour)
{
  $$colour = create_function('$text', 'global $colour;
  return "<FONT COLOR=\'$colour\'>$text</FONT>";');
}

// Let's add a new colour to the list
array_push($colours, 'chartreuse');

foreach ($colours as $colour)
{
  // Checking whether function is defined
  if (!function_exists($$colour))
  {
    // Doesn't exist, so dynamically define it
    $$colour = create_function('$text', 'global $colour;
    return "<FONT COLOR=\'$colour\'>$text</FONT>";');

    // Alternatively, if it exists in a source file, 'include' the file:
    // include 'newcolours.php'
  }

  echo $$colour("Careful with this $colour, James") . "\n";
}

foreach ($colours as $colour) unset($$colour);

}
?>
