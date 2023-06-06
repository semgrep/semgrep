

val x = if true then 1 else 3

val y = 
  if true then 
    4
  else
    5

val x = 
  if (true) 1 else 3

// this test case demonstrates the necessity of 
// indentedExprOrBlockStatSeq...
// it's an indented expr, but it doesn't start a
// blockStatSeq, and it doesn't even have a closing
// DEDENT 
val x = 
  if (true) 
    1 else 3

val x = 
  if (true) 
    1 
  else 3

val x = 
  if (true) 
    1 
  else 
    3

val x = 
  if (true) 
    val x = 2 
    val y = 3 
  else 3

val x = 
  if true then 
    val x = 2 
    val y = 3 
  else 3

val x = 
  if true then 
    x.foo(); 
    val y = 3 
  else 3

val x =
  if true 
  then 1 
  else 2 




 
    