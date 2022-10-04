// If `a.b` is marked clean, then any l-value that starts with `a.b` should
// become clean too!
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
