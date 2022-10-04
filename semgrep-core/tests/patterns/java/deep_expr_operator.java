class Foo {
    void bar() {
        int baz = 0;
        //ERROR: match
        foo(baz + 42);
    }
}
