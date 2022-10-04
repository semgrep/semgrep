void foo() {

  int x = 1;

  if(x) {
    return 1;
  }
label1:
  if(x) {
    goto label1;
  }

  return 3;
}
