// Mark taint on all of `x`, mark some fields as clean
function f() {
    x = source
    x.a = safe
    x.c[i].d = safe
    
    //ruleid: test
    sink(x)
    //ruleid: test
    sink(x.b)
    //ruleid: test
    sink(x.b.c)

    //ok: test
    sink(x.a)
    //ok: test
    sink(x.a.b)

    //ok: test
    sink(x.c[i].d)
    //ok: test
    sink(x.c[i])
    //ok: test
    sink(x.c[j])
    //ok: test
    sink(x.c)
}
