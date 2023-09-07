import A.*;
import A.B.*;

public class Foo {
  public void main () {
    // ERROR:
    foo();

    // ERROR:
    B.foo();

    // ERROR:
    A.B.foo();
  }
}
