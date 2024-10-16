
module 0xcafe::metavar_func_def {
    // ERROR: match
    fun test_metavar_func_def() {
        do_nothing();
    }

    // ERROR: match
    public(friend) fun func_with_args(a: u64, b: u64) : address {
        do_nothing();
        @0x1234
    }
    // ERROR: match
    public fun a_complex_func2<Type> (a: Type, b: Type) : Type  {
        do_nothing(a, b)
    }
}
