public class Class {
  private int x = 5;
  public int y = 5;  
  private int z = 5;


  public void foo() {
    // ruleid: java_private_prop
    return x;
  }

  public void bar() {
    return y; 
  }

  public void qux() {
    z = 3;
    return z; 
  }

  public void foo1() {
    // ruleid: java_private_prop
    return this.x;
  }

  public void bar1() {
    return this.y;
  }

  public void qux1() {
    this.z = 3;
    return this.z;
  }
}
