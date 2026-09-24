// ERROR:
@deprecated
class Foo {
  int foo = 1;
}

// ERROR:
@AnnoBar
class Bar {
  int bar = 1;
}

// OK:
class NoAnno {
  int noanno = 2;
}
