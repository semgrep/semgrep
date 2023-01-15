object Foo {
def foo() {
    //ERROR:
    foo("whatever sequence of chars")
    //ERROR:
    foo(s"whatever sequence of chars")
    //ERROR:
    foo("""whatever sequence of chars""")
}
}
