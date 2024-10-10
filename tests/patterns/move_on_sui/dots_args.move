
module 0xcafe::dots_args {
    fun test_dots_args() {
        //ERROR:
        foo(1,2,3,4,5);
        //ERROR:
        foo(5);
    }
}
