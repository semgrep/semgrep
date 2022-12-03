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
    //ruleid: test
    sink(x.c[j])

    //ok: test
    sink(x.a)
    //ok: test
    sink(x.a.b)

    //ok: test
    sink(x.c[j].d)
    //ok: test
    sink(x.c[j].d.e)
}
