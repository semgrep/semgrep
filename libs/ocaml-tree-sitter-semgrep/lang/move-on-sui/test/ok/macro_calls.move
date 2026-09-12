//# init --edition 2024.alpha

//# publish
module 0x42::m {

    public struct S has drop {
        x: u64,
        y: u64,
        z: u64,
    }

    fun inc_x(self: &mut S, by: u64) {
        self.x = self.x + by;
    }

    macro fun inc_xx($self: &mut S, $by: u64) {
        let self = $self;
        self.x = self.x + $by;
    }

    public fun test0(): u64 {
        let mut s = S { x: 1, y: 2, z: 3 };
        {inc_x(&mut s, 6); s.x} + {inc_x(&mut s, 47); s.x} + {inc_x(&mut s, 117); s.x}
    }

    public fun test1(): u64 {
        let mut s = S { x: 1, y: 2, z: 3 };
        {inc_xx!(&mut s, 6); s.x} + {inc_xx!(&mut s, 47); s.x} + {inc_xx!(&mut s, 117); s.x}
    }

    macro fun inc($x: &mut u64): u64 {
        let x = $x;
        *x = *x + 1;
        *x
    }

    public fun test2(): u64 {
        let mut x = 1;
        x + inc!(&mut x) + inc!(&mut x)
    }

    macro fun inc_scope($x: u64): u64 {
        let mut x = $x;
        x = x + 1;
        x
    }

    public fun test3(): u64 {
        let x = 1;
        x + inc_scope!(x) + inc_scope!(x)
    }

     macro fun call($f: || -> u64): u64 {
        $f()
    }

    public fun test4(): u64 {
        let mut x = 1;
        x + call!(|| {x = x + 1; x}) + call!(|| {x = x + 7; x})
    }
}

//# run
module 0x42::main {
    use 0x42::m;

    public fun main() {
        assert!(m::test0() == 232, 0);
        assert!(m::test1() == 232, 1);
        assert!(m::test2() == 6, 2);
        assert!(m::test3() == 5, 3);
        assert!(m::test4() == 12, 4);
    }

}
