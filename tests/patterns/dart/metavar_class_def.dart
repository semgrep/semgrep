// ERROR:
class Foo {
  int x = 5;

  void m() {
    bar();
  }
}

// ERROR:
class Bar extends Foo {
  int y = 3;
}

// ERROR:
abstract class Baz {
  void doStuff();
}
