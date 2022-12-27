class C {
    public void f() {
        Foo foo;
        // ok: pattern-inside-taint (before taint)
        foo.qux();
        if(true) {
            foo = provider.foo();
            // ruleid: pattern-inside-taint
            foo.qux();
        }
        // ok: pattern-inside-taint (different object)
        baz.qux();
        // ruleid: pattern-inside-taint
        foo.qux();
        foo.bar();
        // ok: pattern-inside-taint (after taint)
        foo.qux();
    }
}
