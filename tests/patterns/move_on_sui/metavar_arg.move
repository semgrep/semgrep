
module 0xcafe::metavar_arg {
    fun test_metavar_arg() {
        //ERROR:
        foo(1,2);

        //ERROR:
        foo(a_very_long_constant_name,
            2);

        //ERROR:
        foo (unsafe(), // indeed
            2);

        //ERROR:
        foo(bar(1,3), 2);

        foo(2,1);
    }
}
