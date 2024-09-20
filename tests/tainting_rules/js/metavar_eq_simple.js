
var source1 = get(A)
var source2 = get(B)
var source3 = get(C)

// source1 is tainted under the assumption that $X = A
//ruleid: test-metavar-eq
sink(A,source1)

// source2 is tainted under the assumption that $X = B
//ruleid: test-metavar-eq
sink(B,source2)

// since source2 is tainted with $X = B, and this sink match assigns $X to A, this sink is not tainted
//OK: 
sink(A,source2)

// Same reason as above
//OK: 
sink(A,source3)

// Same reason as above
//OK: 
sink(B,source3)
