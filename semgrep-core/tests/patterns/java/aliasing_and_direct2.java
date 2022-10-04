import org.foo.Bar;

class x {
  public void main() {
    //ERROR: match
    int x = Bar.newInstance();
  }
}