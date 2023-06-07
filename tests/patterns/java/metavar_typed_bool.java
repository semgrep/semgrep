import java.util.*;

public class metavar_typed_bool {
  public static void main(String[] args) {
    boolean x = true;
    // OK:
    System.out.println(5);
    // MATCH:
    System.out.println(x);
    // MATCH:
    System.out.println(true);
    // MATCH:
    System.out.println(1 == 2);
    // MATCH:
    System.out.println(true == true);
    // MATCH:
    System.out.println(true != false);
    // Even though we can't resolve this name, we can still conclude that the
    // result of a comparison is a boolean. If you comment out this case, and
    // the others with unresolved names, you can run this test file.
    // MATCH:
    System.out.println(unresolved != 3);
    // MATCH:
    System.out.println(3 > 4);
    // MATCH:
    System.out.println(3 >= 4);
    // MATCH:
    System.out.println(3 < 4);
    // MATCH:
    System.out.println(3 <= 4);
    // MATCH:
    System.out.println(true && false);
    // MATCH:
    System.out.println(true || false);
    // MATCH:
    System.out.println(!(1 == 2));

    metavar_typed_bool.overloaded();
    metavar_typed_bool.stdlib();
  }

  public static void overloaded() {
    // Overloaded operators, where the type of the operands determines whether
    // they are boolean operators or bitwise operators

    boolean x = true;

    // When operands are booleans, this is xor
    // MATCH:
    System.out.println(x ^ false);
    // When one operand is unresolved but the other is a boolean, we can assume
    // that the result is a boolean.
    // MATCH:
    System.out.println(unresolved ^ false);
    // MATCH:
    System.out.println(false ^ unresolved);
    // When operands are ints, this is bitwise xor
    // OK:
    System.out.println(1 ^ 2);
    // OK:
    System.out.println(unresolved1 ^ unresolved2);

    // When operands are boolean, these are non-short-circuiting and/or
    // MATCH:
    System.out.println(x | false);
    // MATCH:
    System.out.println(x & false);
    // When operands are ints, thse are bitwise and/or
    // OK:
    System.out.println(3 | 4);
    // OK:
    System.out.println(3 & 4);
  }

  public static void stdlib() {
    String x = "test";
    // OK:
    System.out.println(x);
    // MATCH:
    System.out.println(x.equals("y"));

    List<String> y1 = new ArrayList<>();
    y1.add("test");
    // OK:
    System.out.println(y1.get(0));
    List<Boolean> z1 = new ArrayList<>();
    z1.add(true);
    // MATCH:
    System.out.println(z1.get(0));

    java.util.List<String> y2 = new ArrayList<>();
    y2.add("test");
    // OK:
    System.out.println(y2.get(0));
    java.util.List<Boolean> z2 = new ArrayList<>();
    z2.add(true);
    // MATCH:
    System.out.println(z2.get(0));

    Map<String, String> m1 = new HashMap<>();
    m1.put("x", "y");
    // OK:
    System.out.println(m1.get("x"));

    Map<String, Boolean> m2 = new HashMap<>();
    m2.put("x", false);
    // MATCH:
    System.out.println(m2.get("x"));

    // MATCH:
    System.out.println("asdf".matches("asdf"));

    // MATCH:
    System.out.println(m1.isEmpty());

    java.lang.String y = "test";
    // MATCH:
    System.out.println(y.matches("test"));
  }
}
