import java.util.*;

public class metavar_typed_int {
  // Run with `javac metavar_typed_int.java && java metavar_typed_int`
  public static void main(String[] args) {
    // MATCH:
    System.out.println(1);
    // MATCH:
    System.out.println(1 + 1);
    // MATCH:
    System.out.println(1 - 1);
    // MATCH:
    System.out.println(1 * 1);
    // TODO:
    System.out.println(1 / 1);
    // MATCH:
    System.out.println(1 % 1);
    // OK:
    System.out.println("1");
    // OK:
    System.out.println("1" + "1");

    int x = 5;
    // MATCH:
    System.out.println(x);
    // MATCH:
    System.out.println(x + 1);

    List<String> l = new ArrayList<>();
    l.add("foo");
    // OK:
    System.out.println(l.get(0));
    // MATCH:
    System.out.println(l.size());
  }
}
