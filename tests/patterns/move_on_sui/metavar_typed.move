
module 0xcafe::metavar_typed {
    fun test_metavar_typed(user: &signer, obj: Object<Subscription>) {
        // ERROR: match
        let object_address = object::object_address(obj);
    }
}
