module foo::bar {
    fun f(): u64 {
        1 as u64;
        1 + 2 as u64;
        (1 + 2) as u64;
        1 + (2 as u64);
        v[1 as u64];
        1 as u64 + 2;
        (1 as u64) + 2;
    }

    fun simple(cond: bool, x: u32) {
        (if (cond) x else { x }) as u32;
        (if (cond) x else { x } as u32);
        (loop { break 0 }) as u32;
        (loop { break 0 } as u32);
        ('l: { 0 }) as u32;
        ('l: { 0 } as u32);
        (return) as u32;
        (return as u32);
        loop {
            (break) as u32;
            (break as u32);
            (continue) as u32;
            (continue as u32);
        };
        (0 as u32) as u32;
        (0 as u32 as u32);
    }

    public fun f_imm(s: &S): &u64 { &s.f }

    fun ops(x: u8, y: u8) {
        (x + y as u32);
        (x - y as u32);
        (x * y as u32);
        (x / y as u32);
        (x % y as u32);
        (x & y as u32);
        (x | y as u32);
        (x ^ y as u32);
        (x << y as u32);
        (x >> y as u32);

        (x + y) as u32;
        (x - y) as u32;
        (x * y) as u32;
        (x / y) as u32;
        (x % y) as u32;
        (x & y) as u32;
        (x | y) as u32;
        (x ^ y) as u32;
        (x << y) as u32;
        (x >> y) as u32;
    }


    public struct S has copy, drop { f: u64 }

    fun dotted(cond: bool, mut s: S) {
        (1 + s.f) as u32;
        (1 + s.f as u32);
        (1 + S { f: 0 }.f) as u32;
        (1 + S { f: 0 }.f as u32);
        (*if (cond) { &0 } else { &mut 0 }) as u32;
        // this case still does not work
        // (*if (cond) { &0 } else { &mut 0 } as u32);
        (*if (cond) { &s } else {&mut s}.f_imm()) as u32;
        (*if (cond) { &s } else {&mut s}.f_imm() as u32);
    }
}
