
int main() {
  // ERROR: match
  foo();
  // ERROR: match
  foo(x);
  // ERROR: match
  foo(x, y);

  bar();
  bar(x, y, z);
}