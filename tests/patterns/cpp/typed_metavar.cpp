
int main(int z) {
  // ERROR: match
  int x = 2;
  // ERROR: match
  foo(x);

  double y = 3.0;
  foo(y);

  // ERROR:
  foo(z);

  // ERROR:
  foo(4);
}