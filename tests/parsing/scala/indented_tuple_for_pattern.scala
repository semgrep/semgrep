val x = for
  (a, b) <- 1 
  c      <- 2 
  d      <- 3 
yield (a, b)
