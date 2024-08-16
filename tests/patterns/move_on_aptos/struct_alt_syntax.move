
module 0xcafe::struct_alt_syntax {
    // ERROR: match
    struct AlternativeSyntax {
        a: u64,
        b: u64,
    } has key, store;

    // ERROR: match
    struct NormalSyntax has key, store {
        a: u64,
        b: u64,
    }

    // ok: struct
    struct NormalSyntaxWithoutKey has store {
        a: u64,
        b: u64,
    }

    // ok: struct
    struct AlternativeSyntaxWithoutKey has store {
        a: u64,
        b: u64,
    }

    fun test_struct_alt_syntax() {

    }
}
