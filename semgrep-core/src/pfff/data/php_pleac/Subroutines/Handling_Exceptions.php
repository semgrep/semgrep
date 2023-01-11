# **********************************************************************
# Handling Exceptions
# **********************************************************************
<?php
function pleac_Handling_Exceptions() {
// Unlike in Perl, PHP's 'die' [actually an alias for 'exit'] doesn't throw
// an exception, but instead terminates the script, optionally either
// returning an integer value to the operating system, or printing a message.
// So, the following, does not exhibit the same behaviour as the Perl example

die("some message\n");

// Instead, like so many modern languages, PHP implements exception handling
// via the 'catch' and 'throw' keywords. Furthermore, a C++ or Java programmer
// would find PHP's exception handling facility remarkably similar to those
// of their respective languages. A simple, canonical example follows:

// Usual to derive new exception classes from the built-in, 'Exception',
// class
class MyException extends Exception
{
  // ...
}

// ...

try
{
  // ...
  if ($some_problem_detected) throw new MyException('some message', $some_error_code);
  // ..
}

catch (MyException $e)
{
  ; // ... handle the problem ...
}

// ----------------------------

class FullMoonException extends Exception
{
  // ...
}

// ...

try
{
  // ...
  if ($some_problem_detected) throw new FullMoonException('...', $full_moon_error_code);
  // ..
}

catch (FullMoonException $e)
{
  // ... rethrow the exception - will propagate to higher level ...
  throw $e;
}

}
?>
