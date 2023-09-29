
// not this because it has no part of it thats not the interpolation
val x = "$X"

// ERROR: match
val x = "but $X is good"

// ERROR: match
val x = "anything"

// ERROR: can match the empty literal string too
val x = ""