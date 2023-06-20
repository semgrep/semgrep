import A.*
import A.B.*

public class Foo {
  public void main () {
    // TODO: 
    foo();

    // TODO: 
    B.foo();

    // ERROR:
    A.B.foo();
  }
}