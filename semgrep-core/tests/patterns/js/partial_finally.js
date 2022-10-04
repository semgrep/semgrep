function test() {
    try {
        return 0;
    }
    //ERROR: match
    finally {
        return 1;
    }
}