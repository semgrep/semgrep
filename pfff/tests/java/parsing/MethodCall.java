class X {
  Expr x;

  public void foo() {
    x.index();
    int x;

    // illlegal java?
    x = x.field1;
    bar();
  }
}
