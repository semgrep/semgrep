function foo() {
    x = source()
    //ruleid: test
    foo(x)
    //ok: test
    bar(x)
}

function bar() {
    x = source()
    //ok: test
    foo(x)
    //ruleid: test
    bar(x)
}
