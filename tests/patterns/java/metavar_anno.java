// ERROR:
@AnnoFoo
class Foo {
    int foo = 1;
}

// ERROR:
@Anno1
@Anno2
class FooBar {
    int foobar = 2;
}

@AnnoBar(bar = "bar")
class Bar {
    int bar = 1;
}

class NoAnno {
    int noanno = 2;
}
