public class C {
  // ERROR:
  public foo(int x, int y) {
    int x;
  }

  // ERROR:
  public bar(int x) {
    int x;
  }

  // ERROR:
  public qux() {
    int x;
  }
}