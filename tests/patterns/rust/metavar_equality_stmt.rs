fn foo() {
    //ERROR:
    if (x > 2) {
        return 1;
    } else {
        return 1;
    }
    // nope, not here
    if (x > 2) {
        return 1;
    } else {
        return 2;
    }
}
