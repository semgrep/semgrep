// ERROR:
def f() = "implicit return for function definition without function keyword"

// ERROR:
val _ = () => "implicit return for lambda"

def f() =
  val ignore_me = 0
  // ERROR:
  "implicit return last statement"

def f(x) =
  if x < 0 then
    // ERROR:
    "implicit return if"
  else if x == 0 then
    // TODO:
    "implicit return else if"
  else
    // TODO:
    "implicit return else"

def f(x) =
  val _ =
  if x == 0 then
    "no implicit return because not the last statement"
  else
    "no implicit return because not the last statement"
   
  // ERROR:
  "implicit return last statement after non trivial"

def f() =
  def f() =
    // ERROR:
    "implicit return nested function even when function is not the last statement"

  // ERROR:
  "implicit return last statement after nested function"

def f() =
  try
    // TODO:
    "implicit return in try clause"
  catch
    case e => "no implicit return in catch clause if there is no exception"

def f() =
  try
    "no implicit return in try clause if finally is present"
  catch
    case e => "no implicit return in catch clause if finally is present"
  finally
    // TODO:
    "implicit return in finally clause"

package p:
  def f() =
    // ERROR:
    "implicit return function inside package"

package p:
  def f(implicit_return_parameter) =
    // ERROR:
    implicit_return_parameter

  def g() =
    // ERROR:
    f("implicit return function call")

def f() =
  // ERROR:
  return "explicit return should still work"

