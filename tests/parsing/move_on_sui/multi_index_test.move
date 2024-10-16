module 0x1::m {
    fun f() {
        x[];
        x[1, f()];
        x[1, 2, 3];
    }
}
