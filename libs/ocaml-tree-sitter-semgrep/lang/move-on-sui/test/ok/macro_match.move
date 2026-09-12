//# init --edition 2024.beta

//# publish
module 0x42::m {

    public enum Maybe<T> {
        Just(T),
        Nothing
    }

    macro fun maybe<$A,$B: drop>($b: $B, $f: |$A| -> $B, $ma: Maybe<$A>): $B {
        match ($ma) {
            Maybe::Just(a) => $f(a),
            Maybe::Nothing => $b
        }
    }

    public fun maybe_macro_call_2() {
        let m = maybe!(10, |x| { x }, Maybe::Just(5));
        assert!(m == 5, 1);
        let n = maybe!(10, |x| { x }, Maybe::Nothing);
        assert!(n == 10, 2);
    }
}

//# run
module 0x42::main {
    use 0x42::m;
    fun main() {
        m::maybe_macro_call_2();
    }
}
