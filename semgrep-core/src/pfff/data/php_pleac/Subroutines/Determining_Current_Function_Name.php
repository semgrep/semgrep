# **********************************************************************
# Determining Current Function Name
# **********************************************************************
<?php
function pleac_Determining_Current_Function_Name() {
// AFAICT there is no means of obtaining the name of the currently executing
// function, or, for that matter, perform any stack / activation record,
// inspection. It *is* possible to:
//
// * Obtain a list of the currently-defined functions ['get_defined_functions']
// * Check whether a specific function exists ['function_exists']
// * Use the 'Reflection API'
//
// So, to solve this problem would seem to require adopting a convention where
// a string representing the function name is passed as an argument, or a local
// variable [perhaps called, '$name'] is so set [contrived, and of limited use]

function whoami()
{
  $name = 'whoami';
  echo "I am: {$name}\n";
}

// ------------

whoami();

}
?>
