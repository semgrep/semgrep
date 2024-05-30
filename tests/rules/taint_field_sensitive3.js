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

    // Previously these were not findings, but we now consider that if the
    // `sink` could _potentially_ access tainted data, then it's better to
    // report a finding.
    //ruleid: test
    sink(x.a.b)
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

    // These are OK because no taint can be reached from those fields
    //ok: test
    sink(x.a.b.d)
    //ok: test
    sink(x.a.c)
}
