function foo() {

    var x = { fld1: eval };
    x.fld2 = eval;
    x("console.log('foo')");
}

foo();

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

function funcs() {
    foo(1);
    foo(1, 2);
    foo(1, 2, 3);
}