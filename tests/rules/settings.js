const x = 1;

function test() {
    // not this one when disable constant propagation
    foo(x);
    //ruleid: test-options
    foo(1);
}
