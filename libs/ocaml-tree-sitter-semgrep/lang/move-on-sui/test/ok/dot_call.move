module Move2024::M1 {

    public use fun Self::foo as SomeStruct.f1;
    public use fun Move2024::M1::foo as SomeStruct.f2;

    public struct SomeStruct has drop {
        some_field: u64
    }

    public fun snew(): SomeStruct {
        SomeStruct { some_field: 42 }
    }

    public fun foo(s: &SomeStruct): u64 {
        s.some_field
    }

    public fun bar(s: &SomeStruct, v: u64): u64 {
        s.some_field + v
    }
}


module Move2024::M2 {
    use Move2024::M1::{Self, SomeStruct as SomeStructAlias};
    use Move2024::M1 as M1_ALIAS;
    use fun M1_ALIAS::bar as Move2024::M1::SomeStruct.f3;

    public fun baz() {
        use fun M1::bar as SomeStructAlias.f4;

        let some_struct: SomeStructAlias = M1::snew();
        let val = 42;
        assert!(some_struct.f1() == some_struct.f2(), 0);
        assert!(some_struct.f3(val) == some_struct.f4(val), 0);
    }
}
