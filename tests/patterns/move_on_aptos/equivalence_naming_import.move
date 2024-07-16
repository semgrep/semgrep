module 0xcafe::equivalence_naming_import {
    use std::object::{Self, ConstructorRef as Ref};

    // ERROR: match
    fun test_equivalence_naming_import(input: address) : Ref {
        object::some_func(address);
    }
}
