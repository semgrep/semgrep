
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
    #[test]
    public inline fun a_complex_func<Type> (a: Type, b: Type) : Type pure {
        do_nothing(a, b)
    }
}

script {
    // ERROR: match
    entry fun script_func() {
        0xcafe::metavar_func_def::test_metavar_func_def();
        0xcafe::metavar_func_def::func_with_args(1, 2);
        0xcafe::metavar_func_def::a_complex_func(1, 2);
    }
}
