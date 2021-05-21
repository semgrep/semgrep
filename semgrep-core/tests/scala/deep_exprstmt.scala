object Foo {

def foo() {
    //ERROR: match
    bar()
    //ERROR: match
    x = bar()
    //ERROR: match
    print(bar())
    //ERROR: match
    return bar()
}
}
