// If `x.a` is marked clean, then any l-value that starts with `x.a` should
// become clean too!
function f() {
    x.a.b = source
    x.a.c = source

    //ruleid: test
    sink(x.a.b)
    //ruleid: test
    sink(x.a.c)
    //ruleid: test
    sink(x.a.b.x)

    // After this, all the above ones become clean!
    x.a = safe
    //ok: test
    sink(x.a.b)
    //ok: test
    sink(x.a.c)
    //ok: test
    sink(x.a.b.x)
}
