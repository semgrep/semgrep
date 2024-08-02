
module 0xcafe::misc_address_value {
    fun test_misc_address_value() {
        // ERROR: match
        let address_val = @0xcafe;

        // ok: byte_string
        let byte_string = b"0xcafe";
    }
}
