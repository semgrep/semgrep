class Foo {
  private final String FIELD = "password";

  void test() {
    //ERROR: match
    foo(FIELD);

    //ERROR: match
    foo(this.FIELD);
  }
}
