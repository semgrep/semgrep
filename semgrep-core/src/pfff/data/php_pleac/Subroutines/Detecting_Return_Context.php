# **********************************************************************
# Detecting Return Context
# **********************************************************************
<?php
function pleac_Detecting_Return_Context() {
// PHP can be described as a dynamically typed language because variables serve
// as identifiers, and the same variable may refer to data of various types.
// As such, the set of arguments passed to a function may vary in type between
// calls, as can the type of return value. Where this is likely to occur type
// checking should be performed either / both within the function body, and
// when obtaining it's return value. As for Perl-style 'return context', I
// don't believe it is supported by PHP

// Can return any type
function mysub()
{
  // ...
  return 5;
  // ...
  return array(5);
  // ...
  return '5';
}

// Throw away return type [i.e. returns a 'void' type ?]
mysub();

// Check return type. Can do via:
// * gettype($var)
// * is_xxx e.g. is_array($var), is_muneric($var), ...
$ret = mysub();

if (is_numeric($ret))
{
  ; // ...
}

if (is_array($ret))
{
  ; // ...
}

if (is_string($ret))
{
  ; // ...
}

}
?>
