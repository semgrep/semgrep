
module 0xcafe::metavar_cond {
    fun test_metavar_cond(): u32 {
        let u : u128 = 0xabcdef;

        // ERROR: match
        if (u >= 0xaaa) {
            foo();
        } else {
            return 0;
        }
    }
}
