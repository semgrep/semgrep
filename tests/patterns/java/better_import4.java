import A.B.*

public class Foo {
  public void main () {
    // TODO: 
    C.D.foo();

    // OK:
    D.foo();
    
    // OK:
    B.C.D.foo();
  }
}