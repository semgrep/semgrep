module test::mod {
    struct Inner {
        a: u64,
        b: u64,
    }

    struct Outer {
        inner: Inner,
        addr: address,
    }

    fun test() {
        let f = Outer {
            inner: Inner { a: 1, b: 2 },
            addr: 0x42,
        };
        
        // ruleid: pattern_move_let_bind
        let Outer {
            inner: Inner { a, b },
            addr,
        } = f;
    }
}