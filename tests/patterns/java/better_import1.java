import A;

public class Foo {
  public void main () {
    // ERROR:
    A.foo();

    // OK:
    foo();
  }
}
