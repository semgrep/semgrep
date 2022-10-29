// Mark taint on all of `x`, remember that only `x.a` is clean
function f() {
    x = source
    x.a = safe
    
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
}
