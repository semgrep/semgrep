# **********************************************************************
# Prototyping Functions
# **********************************************************************
<?php
function pleac_Prototyping_Functions() {
// Whether PHP is seen to support prototyping depends on the accepted
// definition of this term:
//
// * Prototyping along the lines used in Ada, Modula X, and even C / C++,
//   in which a function's interface is declared separately from its
//   implementation, is *not* supported
//
// * Prototyping in which, as part of the function definition, parameter
//   information must be supplied. In PHP a function definition neither
//   parameter, nor return type, information needs to be supplied, though
//   it is usual to see a parameter list supplied [indicates the number,
//   positional order, and optionally, whether a parameter is passed by
//   reference; no type information is present]. In short, prototyping in
//   PHP is optional, and limited

function func_with_one_arg($arg1)
{
  ; // ...
}

function func_with_two_arg($arg1, $arg2)
{
  ; // ...
}

function func_with_three_arg($arg1, $arg2, $arg3)
{
  ; // ...
}

// The following may be interpreted as meaning a function accepting no
// arguments:
function func_with_no_arg()
{
  ; // ...
}

// whilst the following may mean a function taking zero or more arguments
function func_with_no_arg_information()
{
  ; // ...
}

}
?>
