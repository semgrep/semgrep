
// this test reveals an existing bug due to a shift/reduce conflict
//var t1 = (x,1)

// this test shows why shifting on ) instead of reducing can be bad
var t2 = (x) + y

// this test shows why shifting on ? instead of reducing can be bad
var t3 = (x?y:z);

// this test shows why shifting on = instead of reducing can be bad
var t3 = (x=0) + y;
