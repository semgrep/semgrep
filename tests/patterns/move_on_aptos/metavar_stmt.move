
module 0xcafe::metavar_stmt {
    const var: u32 = 0x1234;
    
    fun test_metavar_stmt() {
        // ERROR: match
        if (var > 2) {
            return;
        } else {
            abort {
                0x1234
            };
        }
    }
}
