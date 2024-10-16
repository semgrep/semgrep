
module 0xcafe::dots_nested_stmts {
    fun test_dots_nested_stmts() {
        // ERROR: match
        if (x == some_cond) {
            print(b"matched");
        } else {
            print(b"not matched");
        }
    }
}
