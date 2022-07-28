// If `a.b.c` is tainted, then all its prefixes are also tainted
function f() {
    a.b.c = source

    //ruleid: test
    sink(a.b.c)

    // These are ok because we have not enabled propagation of taint up through fields, to avoid FPs
    //ok: test
    sink(a.b)
    //ok: test
    sink(a)
}