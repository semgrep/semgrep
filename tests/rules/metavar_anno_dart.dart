// ruleid: metavar-anno-dart
@deprecated
class Foo {
  int x = 0;
}

// ruleid: metavar-anno-dart
@override
class Bar {
  int y = 0;
}

// ok: metavar-anno-dart
class Plain {
  int z = 0;
}
