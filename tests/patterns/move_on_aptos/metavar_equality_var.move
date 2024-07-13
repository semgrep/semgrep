
module 0xcafe::metavar_equality_var {
    fun test_metavar_equality_var() {
        // ERROR: match
        let something = source();
        sink(something);
    }
}
