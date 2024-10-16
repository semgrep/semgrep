module foo::bar {
    // Imports
    use std::vector::foo;
    use fun Self::foo as X.f1;
    use fun a::m::foo as X.f2;
    use fun foo as Self::X.f3;
    use fun foo as a::m::X.f4;
    use fun foo as X.f5;
    public use fun Self::foo as X.g1;
    public use fun a::m::foo as X.g2;
    public use fun foo as Self::X.g3;
    public use fun foo as a::m::X.g4;
    public use fun foo as X.g5;

    use std::{
        ascii::{Self, String},
        vector::push_back,
        vector::pop_back as p,
        coin,
        object as O,
    };

    // Types
    public struct X {}
    public struct Old<T: key + copy + store> has key {
        x: u64
    }
    public struct NewPost<T: key + copy + store> has key (u64)
    public struct NewPoster<T: key + copy + store>(u64) has key, store;
    public struct None()
    public enum NewEnum {
        V(),
        V1(u64, bool),
        V2 { x: u64, y: bool },
        V3
    }
    public enum NewEnum<T: key + copy + store> has key {
        NewVariant(u64),
        VariantNoParams,
        VariantEmptyParams(),
        VariantNamedParams { x: u64 },
    }

    public struct X {
        `for`: u64,
        `let`: u64,
        `struct`: u64,
    }

    #[syntax(index)]
    public fun new<T>(mut x: T): T { }

    public fun `let`(mut `mut`: u64) {
        let `let` = `mut`;
        let `for` = `mut` + `let` + `let` + `mut`;
        let `struct` = `let` + `for` + `let` + `mut`;
        let `enum` = `struct` + `let` + `for` + `let` + `mut`;
        let `let` = `let` + `for` + `let` + `mut`;
        let `let` = `let` * `mut` + `let` / `for` - `struct`;
    }

    public fun foo() {
        let x = (x: C<u64>).y;
        *if (cond) { &0 } else { &mut 0 };
        ()
    }

    fun f(): u64 {
        1 as u64;
        1 + 2 as u64;
        (1 + 2) as u64;
        1 + (2 as u64);
        v[1 as u64];
        1 as u64 + 2;
    }

    public fun new_let_mut(): u64 {
        let mut x = 0;
        x = 1;
        let t = x.new();
        let t = x.new(a, b, t.y());
        let Old { mut y } = x.new();
        let NewPost(y) = x.new();
        let NewPoster(mut y) = x.new();
        let NewPoster(mut y, z, i) = x.new();
        let NewPoster<T>(mut y, z, i) = x.new();
        let NewPoster::Variant<T>(mut y, z, i) = x.new();
        match (s) {
            NewPoster::Variant(x) => something,
            NewPoster::Variant{ x } if true => something,
            y @ bar => @0x1,
        };
        x.foreach!(|y| { x = y; });
        assert!(x == 1, 6);
        x
    }

    // blocks
    public fun block() {
        'a: {
            return 'a x;
            break 'a x;
            return'a x.foo!();
            break'a { x = x + 1; x };
            continue 'a;
        };
        'a: loop { };
        'a: while (true) { };
        while (true) 'a: { };
        // TODO: fix precedence of this
        if (true) 1 else 'a: { 2 } + 1;
    }

    // Macros
    macro fun ignore(
        _: None,
    ) {}

    macro fun for($start: u64, $stop: u64, $body: |u64|) {
        let mut i = $start;
        let stop = $stop;
        while (i < stop) {
            $body(i);
            i = i + 1
        }
    }

    macro fun for_each<$T>($v: &vector<$T>, $body: |&$T|) {
        let v = $v;
        let mut i = 0;
        let n = v.length();
        while (i < n) {
            $body(v.borrow(i));
            i = i + 1
        }
    }

    macro fun new<$T>($len: u64, $f: |u64| -> $T): vector<$T> {
        let len = $len;
        let mut v = vector[];
        for!(0, len, |i| v.push_back($f(i)));
        v
    }

    macro fun sum($v: &vector<u64>): u64 {
        let mut s = 0;
        for_each!($v, |i| s = s + *i);
        s
    }

    fun t() {
        None().ignore!()
    }

    public entry fun main() {
        let v = new!(10, |i| i);
        assert!(sum!(&v) == 45, 0);
    }

    public struct Cup<phantom T> has drop {}
    public macro fun foo(
        _: ||,
        _: || -> (),
        _: || -> u64,
        _: || -> (u64),
        _: || -> (u64, bool),
        _: |&u64|,
        _: |&u64| -> (),
        _: |&u64| -> u64,
        _: |&u64| -> (u64),
        _: |&u64| -> (u64, bool),
        _: |bool, address|,
        _: |bool, address| -> (),
        _: |bool, address| -> u64,
        _: |bool, address| -> (u64),
        _: |bool, address| -> (u64, bool),
        _: |bool, address| -> (u64, bool, &u64),
        _: || -> || -> ||,
        _: || -> || -> || -> || -> (),
        _: || -> | | -> || -> | | -> u64,
        _: | | -> || -> | | -> || -> (u64),
        _: Cup<||>,
        _: Cup<|| -> u64>,
    ) {}

    macro fun call<$T>($f: || -> $T): $T {
        $f()
    }

    fun t() {
        call!(|| -> u64 'a: { 0 });
    }

    fun t() {
        call!(|| -> () { });
        call!(|| -> () { () });
        call!(|| -> u64 { 0 });
        call!(|| -> (u64, u8) { (0, 0) });
    }
}
