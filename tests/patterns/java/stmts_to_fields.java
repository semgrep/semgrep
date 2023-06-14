
public class A {
  // ERROR: match
  public int foo = 1;
  public int bar = 2;

  public void main() {
    // ERROR: match
    int foo = 1;
    int bar = 2;
  }
}