void main() {

  vector<int> x =
    vector<int>();

  vector<int> x =
    vector<int>(2);

  vector<int> x(2);

  stringstream ss (stringstream::in | stringstream::out);

  foo1 x(foo2);

  foo1 x(foo2());
  foo1 x(foo2, foo3);

  foo1 x(foo2, foo3[i]);

  foo1 x(&foo2);

  string& x("foo");

  foo1 x(this);

  int nAttempt(0);

  x = int(2);

  foo1 x(foo1, foo2, foo3);

  foo1 x((const char*)foo1, foo2);
}
