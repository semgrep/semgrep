class Test {

  private String x = source();

  void test() {
    //ruleid: tainting
    sink(x);
  }

}
