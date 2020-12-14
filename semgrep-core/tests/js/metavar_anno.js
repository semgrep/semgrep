// ERROR:
@Anno1
class Foo {
    foo() {
        var foo = 1;
    }
}

// ERROR:
@Anno1()
@Anno2()
class FooBar {
    foo() {
        var foo = 2;
    }
}

@AnnoVar("Bar")
class Bar {
    bar() {
        var bar = 1;
    }
}

class NoAnno {
    noanno() {
        var no_anno = 1;
    }
}
