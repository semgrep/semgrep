//ERROR: match
val x = for (w <- u if foo) yield w

//ERROR: match
val y = for (a <- b; w <- u if foo; c <- d) yield w

//OK:
val z = for (a <- b; w <- u if bar) yield w