let foo x =
  match x with
  #if XXX
  |    1 -> 1
            #else
  |    1 -> 2
            #endif
