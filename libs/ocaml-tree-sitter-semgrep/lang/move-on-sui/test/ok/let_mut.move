module Move2024::let_mut {

    public fun foo(mut p: u64): u64 {
        p = 42;
        let mut v = 7;
        v = v + p;
        v
    }
}
