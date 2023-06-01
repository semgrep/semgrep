import java.util.*;

public class Foo {
  public List<String> strlist;
  public List<Integer> intlist;
  public Foo() {
    this.strlist = new ArrayList<>();
    this.intlist = new ArrayList<>();
  }
  public void f() {
    // MATCH:
    this.strlist.add("thing");
    // MATCH:
    this.intlist.add(5);

    // MATCH:
    strlist.add("thing");
    // MATCH:
    intlist.add(5);
  }
}
