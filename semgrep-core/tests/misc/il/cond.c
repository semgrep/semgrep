void foo() {
  int x;
  x = eval("true") ? foo() : bar();
}
