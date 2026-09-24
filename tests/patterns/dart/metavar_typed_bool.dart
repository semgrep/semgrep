void main() {
  bool a = true;
  bool b = false;
  int n = 1;

  //ERROR: match
  foo(a && b);

  //ERROR: match
  foo(a || b);

  //OK:
  foo(n);
}
