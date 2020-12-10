// ERROR:
@Anno1
class Foo {
    var foo = 1;
}

// ERROR:
@Anno1
@Anno2
class FooBar {
    var foo = 2;
}

@AnnoVar("Bar")
class Bar {
    var bar = 1;
}

class NoAnno {
    var no_anno = 1;
}
