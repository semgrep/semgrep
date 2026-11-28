class C {
    void Spread() {
        int[] xs = source();
        int[] a = [1, ..xs];
        // ruleid: taint
        sink(a);
    }

    void Clean() {
        int[] xs = [2];
        int[] a = [1, ..xs];
        // ok: taint
        sink(a);
    }
}
