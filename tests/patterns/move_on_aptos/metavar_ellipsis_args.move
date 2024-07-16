
module 0xcafe::metavar_ellipsis_args {
    fun test_metavar_ellipsis_args() {
        // ERROR: match
        foo(1, 2, 3, 1, 2);
        foo(1, 2, 3, 4, 5);
    }
}
