/// Module: kek
module kek::kek {
    public struct Kek {
        a: u8,
        b: u64,
    }

    public fun destroy(k1: Kek, k2: Kek) {
        let Kek { a: _, .. } = k1;
        let Kek { .. } = k2;
    }
}
