// If `a.b` is marked clean, then anything that is prefix of `a.b` should become clean
function f() {
    a.b.c = source
    a.b.d = source
    //ruleid: test
    sink(a.b.c.x)
    a.b = safe
    //ok: test
    sink(a.b)
    //ok: test
    sink(a.b.c)
    //ok: test
    sink(a.b.c.d)
    //ok: test
    sink(a.b.c.x)
}