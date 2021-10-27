
var source1 = get(A)
var source2 = get(B)
var source3 = get(C)

//ERROR: source1 is tainted under the assumption that $X = A
sink(A,source1)

//ERROR: source2 is tainted under the assumption that $X = B
sink(B,source2)

//OK: since source2 is tainted with $X = B, and this sink match assigns $X to A, this sink is not tainted
sink(A,source2)

//OK: Same reason as above
sink(A,source3)

//OK: Same reason as above
sink(B,source3)