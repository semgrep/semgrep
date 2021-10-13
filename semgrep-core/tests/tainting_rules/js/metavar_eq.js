
var source1 = get(A)
var source2 = get(B)
var source3 = get(C)

//ERROR:
sink(A,source1)

//ERROR:
sink(B,source2)

//OK:
sink(A,source2)

//OK:
sink(A,source3)

//OK:
sink(B,source3)