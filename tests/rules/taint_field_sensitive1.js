// Separately mark taint/no taint on each field of `x`
function f() {
    x.a = source // only x.a or its extension are tainted after this
    x.b = safe
    x.c = source // only x.c or its extension are tainted after this
    x.d[i] = source // only x.d is tainted

    // x.a is tainted
    //ruleid: test
    sink(x.a)
    //ruleid: test
    sink(x.a.b)

    // x.c is tainted
    //ruleid: test
    sink(x.c)
    //ruleid: test
    sink(x.c.d)

    //ruleid: test
    sink(x.d[i])
    //ruleid: test
    sink(x.d[j])
    // ruleid: test
    sink(x.d)

    // x itself and other fields of x are not tainted
    //ruleid: test
    sink(x)
    //ok: test
    sink(x.b)
    //ok: test
    sink(x.b.c)
}
