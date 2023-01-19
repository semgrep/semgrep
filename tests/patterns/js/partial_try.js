function test() {
    //ERROR: match
    try {
        foo();
    }
    catch (e) {
        return e;
    }
}