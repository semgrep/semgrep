import A.B;

public class Foo {
  public void main () {
    // ERROR:
    B.foo();

    // OK:
    A.foo();

    // OK:
    C.foo();
  }
}
