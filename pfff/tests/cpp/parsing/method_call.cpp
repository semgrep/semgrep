
#include <stdio.h>

class Foo {
public:
  void foo() { printf("foo\n"); }
};

int main()
{
  Foo x;
  x.foo();
}
