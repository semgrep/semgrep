
var source1 = get(A)
var source2 = get(B)

//ERROR:
sink(A,source1)

//ERROR:
sink(B,source2)

//OK:
sink(A,source2)