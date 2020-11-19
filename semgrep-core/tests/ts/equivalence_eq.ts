function test_equal() {
    a = 1;
    b = 2;
    //ERROR: match
    if (a+b == a+b)
        return 1;

    if (a+b != a+b)
        return 1;
    return 0;
}

//TODO: test also === vs !==, Javascript/Typescript specific
// 1+2 !== 1+2;
function bar() {
    var x;
    if(1 === 2) {
    }
    if(1+2 === 1+2) {
    }
    if(1+2 === 1+2) {
        foo();
    }
    if(x === x) {
        foo();
    }
}
