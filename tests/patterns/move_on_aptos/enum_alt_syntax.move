
module 0xcafe::enum_alt_syntax {
    // ERROR: match
    enum AlternativeSyntax {
        A {
            a: u64,
            b: u64,
        },
        B(u64, u64),
        C,
    } has key, store;

    // ERROR: match
    enum NormalSyntax has key, store {
        Ok(u64),
        Err(u64),
    }

    // ok: enum
    enum NormalSyntaxWithoutKey has store {
        Ok(u64),
        Err(u64),
    }

    // ok: enum
    enum AlternativeSyntaxWithoutKey has store {
        A {
            a: u64,
            b: u64,
        },
        B(u64, u64),
        C,
    }

    fun test_enum_alt_syntax() {

    }
}
