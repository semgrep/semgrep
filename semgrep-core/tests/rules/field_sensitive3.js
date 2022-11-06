// Quite similar to field_sensitive1 ...
function f() {
    x.a.b.c = source

    //ruleid: test
    sink(x.a.b.c)
    //ruleid: test
    sink(x.a.b.c.d)

    // These are OK because we have not enabled propagation of taint up through
    // fields, to avoid FPs
    //ok: test
    sink(x.a.b.d)
    //ok: test
    sink(x.a.b)
    //ok: test
    sink(x.a.c)
    //ok: test
    sink(x.a)
    //ok: test
    sink(x)
}
