// Mark taint on all of `a`, remember that only `a.b` is clean
function f() {
    a = source
    a.b = safe
    
    //ruleid: test
    sink(a)

    //ruleid: test
    sink(a.c)

    //ok: test
    sink(a.b)
}