module Move2024::implicit_uses {

    public struct SomeStruct {
        opt: Option<u8>,
    }

    public fun foo(): SomeStruct {
        SomeStruct { opt: option::some(42) }
    }
}
