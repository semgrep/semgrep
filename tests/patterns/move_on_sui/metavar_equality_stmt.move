
module 0xcafe::metavar_equality_stmt {
    fun test_metavar_equality_stmt() {
        // ERROR:
        if (x > 2) {
            foo();
        } else {
            foo();
        };

        if (x < 2) {
            bar();
        } else {
            foo();
        };
    }
}
