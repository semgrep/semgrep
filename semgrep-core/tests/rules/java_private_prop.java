public class Class {
  private int x = 5;
  public int y = 5;  
  private int z = 5;


  public void foo() {
    // ruleid: java-private-prop
    return x;
  }

  public void bar() {
    return y; 
  }

  public void qux() {
    z = 3;
    return z; 
  }
}