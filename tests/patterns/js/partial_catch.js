function test() {
    try {
        foo();
    }
    //ERROR: match
    catch (e) {
        return e;
    }
}