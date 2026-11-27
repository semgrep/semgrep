class C {
    void M() {
        var x =
#if A
            ""
#else
            source()
#endif
            ;
        // ruleid: taint
        sink(x);
    }
}
