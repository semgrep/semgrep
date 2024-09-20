

var source = cond ? get(A) : get(B)


//Since source could be tainted with either $X = A or $X = B, both of these sinks should be tainted

//ruleid: test-metavar-eq
sink(A,source)

//ruleid: test-metavar-eq
sink(B,source)
