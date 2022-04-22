function foo() {
    x = source()
    //ruleid: test
    foo(x)
    //ruleid: test
    bar(x)
}

function bar() {
    x = source()
    //ruleid: test
    foo(x)
    //ruleid: test
    bar(x)
}
