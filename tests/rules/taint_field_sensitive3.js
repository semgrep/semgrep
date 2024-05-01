// Quite similar to field_sensitive1 ...
function f() {
    x.a.b.c = source
    x.d.e[i].f.g[j].h = source

    //ruleid: test
    sink(x.a.b.c)
    //ruleid: test
    sink(x.a.b.c.d)
    //ruleid: test
    sink(x.d.e[k].f.g[l].h)
    //ruleid: test
    sink(x.d.e[k].f.g[l].h.i)

    // These are OK because we have not enabled propagation of taint up through
    // fields, to avoid FPs
    //ok: test
    sink(x.a.b.d)
    //ruleid: test
    sink(x.a.b)
    //ok: test
    sink(x.a.c)
    //ruleid: test
    sink(x.a)
    //ruleid: test
    sink(x.d.e[i].f.g[j])
    //ruleid: test
    sink(x.d.e)
    //ruleid: test
    sink(x.d)
    //ruleid: test
    sink(x)
}
