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
    // Previously Pro did not report a finding here, but we now reverted that,
    // see comment below on the `sink(x)` case.
    // ruleid: test
    sink(x.d)

    // Previously we did not report a finding here, but it's actually desirable
    // to report it, because the `sink` is opaque and it could access `x.a`.
    //ruleid: test
    sink(x)

    // the other fields of `x` are not tainted
    //ok: test
    sink(x.b)
    //ok: test
    sink(x.b.c)
}
