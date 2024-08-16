
module 0xcafe::pattern_enum {
    struct SimpleStruct {
        a: u64,
        b: u64,
    }

    enum Option<T> has key, store {
        Some {
            value: T,
        },
        None,
    }

    enum TestEnum {
        A,
        B {
            what: Option<u64>,
            element: SimpleStruct,
        },
        C,
    }



    fun test_pattern_enum() {
        let b = TestEnum::B {
            what: Option::Some { value: 42 },
            element: SimpleStruct { a: 1, b: 2 },
        };

        // ERROR: match
        match (b) {
            TestEnum::A => {
                assert(false, b"A");
            },
            TestEnum::B { what: Option::Some { value }, element } if value > 100 => {
                assert(false, b"B");
            },
            TestEnum::C if false => {
                assert(false, b"C");
            },
            _ => {
                assert(false, b"C");
            },
        }
    }
}
