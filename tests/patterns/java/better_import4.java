import A.B.*;

public class Foo {
  public void main () {
    // ERROR:
    C.D.foo();

    // OK:
    D.foo();

    // OK:
    B.C.D.foo();
  }
}
