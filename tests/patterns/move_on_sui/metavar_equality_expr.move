
module 0xcafe::metavar_equality_expr {
    fun test_metavar_equality_expr() : u32 {
        let a = 1;
        let b = 2;

        // ERROR: match
        if (a + b == a + b)
            return 1;
        return 0;
    }
}
