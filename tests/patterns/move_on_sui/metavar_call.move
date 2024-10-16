
module 0xcafe::metavar_call {
    fun test_metavar_call() : u32 {
        //ERROR:
        foo(1, 2);

        return 1;
    }
}
