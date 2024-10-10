
module 0xcafe::deep_expr_operator {
    struct SomeStruct {
        field1: (u64, u64),
        field2: u64,
    }

    fun test_deep_expr_operator() {
        let field2 = 43;

        // ERROR: match
        let tmp = foo(SomeStruct {
            field1: (42, 0),
            field2
        });
    }
}
