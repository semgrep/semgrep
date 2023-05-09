
val x = 
  for 
    _ <- 2 
    x <- 5
    y <- foo(a, b, c)
  yield
    6

val y = 
  for 
    y <- foo(a, b, c)
    if true
    if false
  yield
    6

val w = 
  for {
    _ <- 2 
  }
  yield
    6

val z = 
  for (
    _ <- 2 
  )
  yield
    6