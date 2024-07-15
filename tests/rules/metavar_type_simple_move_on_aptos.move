module 0xcafe::TestModule {
    use std::object::{Self, ConstructorRef};

    fun test_metavar_func_def(ref: ConstructorRef, obj: object) {
        // ruleid: metavar_type_simple_move_on_aptos
        ref.do_nothing();

        // ruleid: metavar_type_simple_move_on_aptos
        obj.do_nothing();
    }
}