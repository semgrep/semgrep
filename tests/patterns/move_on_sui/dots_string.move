
module 0xcafe::dots_string {
    fun test_dots_string() {
        //ERROR:
        foo(b"whatever sequence of chars");
        //ERROR:
        foo(b"whatever sequence of chars");
    }
}
