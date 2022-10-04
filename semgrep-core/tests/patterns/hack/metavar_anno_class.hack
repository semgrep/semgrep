// ERROR:
<<Deprecated>>
class Foo {
    const int foo = 1;
}

// ERROR:
<<Entry>>
class FooBar {
    const int foobar = 2;
}

// ERROR:
<<Entry("With Args")>>
class Bar {
    const int bar = 1;
}

class NoAnno {
    const int noanno = 2;
}
