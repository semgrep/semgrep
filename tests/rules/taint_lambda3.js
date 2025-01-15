function test() {
    var x;
    foo((key) => {
        x = source();
    });
    //ruleid: test
    sink(x);
}
