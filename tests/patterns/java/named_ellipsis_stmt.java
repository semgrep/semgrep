public class C {
  public foo(int x, int y) {
    // ERROR:
    foo();
    bar();

    // ERROR:
    foo();
    int x;
    bar();

    // ERROR:
    foo();
    int x;
    int y;
    bar();
  }
}