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
}
