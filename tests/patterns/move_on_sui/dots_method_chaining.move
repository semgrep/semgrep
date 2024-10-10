
module 0xcafe::dots_method_chaining {
    fun test_dots_method_chaining() {
        let f;
        let o = generate();
        //ERROR: match
        f = o.foo().m().h().bar().z();

        //ERROR: match
        f = o.foo().bar();

        f = o.foo().m().h().z();

        //ERROR: match $O can match o.before()
        f = o.before().foo().m().h().bar().z();
    }
}
