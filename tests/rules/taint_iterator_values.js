function test() {
    const myArray = [tainted, 'ok', 'ok'];
    const iterator = myArray.values();
    //ruleid: test
    sink(iterator);
}