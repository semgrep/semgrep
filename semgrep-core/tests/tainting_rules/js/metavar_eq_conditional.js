

var source = cond ? get(A) : get(f(A))
 
//ERROR:
sink(f(A),source)

//OK:
sink(B,source)