

var source1 = cond ? get(A) : get(B)


//Since source1 could be tainted with either $X = A or $X = B, both of these sinks should be tainted

//ERROR:
sink(A,source1)

//ERROR:
sink(B,source1)


var source2 = cond ? get(A) : get(f(A))
 
// Since a sink can either assign $X = A or $X = f(A), and source can be tainted from either of these
// there are two possible ways that this expression could be considered a tainted sink.
//ERROR: 
sink(f(A),source2)

//OK:
sink(B,source2)