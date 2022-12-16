
// need a sentinel for the first declaration
foo0 *x;

int x;

foo1 x;

// ugly, but allowed ...
foo2 foo2;

foo3 *x;

const foo4 *const x;

foo5 **x;

int x = (foo6) y;

int x = (foo7 *) y;

int x = (foo8) 1;

int x = (foo9 **) y;

int foo(foo10 *, foo11 &);

int foo(long, foo11);

foo *bar(int);

foo bar(foo10* x);

class X {
public:
  virtual foo *bar(int);
};


//class Foo {
//  Foo & operator=(const Foo &);
//};

typedef const value* result;

void foo() throw(Exception) {
}
