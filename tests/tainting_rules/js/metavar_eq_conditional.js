

var source = cond ? get(A) : get(B)


//Since source could be tainted with either $X = A or $X = B, both of these sinks should be tainted

//ERROR:
sink(A,source)

//ERROR:
sink(B,source)
