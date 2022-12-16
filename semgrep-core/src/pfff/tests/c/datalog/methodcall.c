

struct Foo {
  int (*f)(int x);
};

struct Foo *x;

int test(int x) {
  return 42;
}

void main() {
  x = malloc(sizeof(struct Foo));
  x->f = &test;
  int res;
  res = x->f(2);
}
