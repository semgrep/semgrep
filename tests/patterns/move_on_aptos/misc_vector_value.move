
module 0xcafe::misc_vector_value {
    fun test_misc_vector_value(user: &signer, obj: Object<Subscription>) {
        // ERROR: match
        let object_address = vector[0xcafe::object::object_address(&obj)];

        // ERROR: match
        let vec_3 = vector[0x1234, 0x5678, 0x9abc];

        // ERROR: match
        let vec_0 = vector[];

        // ERROR: match
        let vec_1 = vector[0x1234];
    }
}
