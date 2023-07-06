// if this does not properly translate the macro args,
// `foo` will end up as an OtherArg, which will not survive
// to the IL, and thus be unable to transmit taint. 
let res = format!("blah", foo);
// ruleid: macro-arg-taint
sink(res);

// this shouldn't work because it has two commas in a row,
// which will fail to recognize this as valid macro args, so
// it will end up in OtherArg
let res2 = format!("blah", , foo);
sink(res2);