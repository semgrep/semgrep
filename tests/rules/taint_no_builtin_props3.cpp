class Point {
public:
  Point() : x(0), y(0) {}
  Point(int x, int y) : x(x), y(y) {}

  int x;
  int y;
  Point *other;
};

class Foo {
};

struct container {
  Point *point;
};

Foo *f11() {
  Point *p = new Point();
  delete p;
  // ruleid: test
  Foo *foo = new Foo(p);
  // ok: test
  return foo;
}
