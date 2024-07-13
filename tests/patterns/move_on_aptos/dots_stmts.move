
module 0xcafe::dots_stmts {
    fun test_dots_stmts() {
        // ERROR: match
        let var = get();
        ...
        eval(var);
    }
}
