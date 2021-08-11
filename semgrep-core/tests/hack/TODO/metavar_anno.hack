// ERROR:
<<Deprecated>>
class Foo {
    int foo = 1;
}

// ERROR:
<<Entry>>
class FooBar {
    int foobar = 2;
}

<<Entry("With Args")>>
class Bar {
    int bar = 1;
}

class NoAnno {
    int noanno = 2;
}
