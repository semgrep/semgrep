module sui::foo {
    public struct Bar { x: u64 }

    fun f() { }

    fun g(x: u64): u64 { x }

    fun h(x: Bar): u64 { x.x }

    fun j(x: Bar): u64 { 
        let mut x = x.x();
        x.foo!()
    }
}
