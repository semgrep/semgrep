<?php
// ERROR:
#[AnnoFoo]
class Foo {
}

// ERROR:
#[Anno1, Anno2]
class FooBar {
}

#[AnnoBar("bar")]
class Bar {
}

class NoAnno {
}
