// Separately mark taint/no taint on each field of `a`
function f() {
    a.b = source
    a.c = safe

    //ruleid: test
    sink(a.b)

    //ok: test
    sink(a.c)
}