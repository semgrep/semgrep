module Move2024::structs {

    public struct SomeStruct {} has drop, copy;

    public struct Positional(u64, SomeStruct) has drop, copy;

    public fun foo(positional: Positional): (u64, SomeStruct) {
        (positional.0, positional.1)
    }

}
