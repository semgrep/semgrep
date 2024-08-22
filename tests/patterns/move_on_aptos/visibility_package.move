
module 0xcafe::visibility_package {
    fun test_visibility_package() {

    }

    // ERROR: match
    public(package) fun some_func() : u64 {
        let a = 1;
        a
    }
}
