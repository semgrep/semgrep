public class boxing_equivalences_boolean {
  public static void main(String[] args) {
    boolean b1 = true;
    Boolean b2 = false;
    java.lang.Boolean b3 = true;
    // MATCH:
    System.out.println(b1);
    // MATCH:
    System.out.println(b2);
    // MATCH:
    System.out.println(b3);
    // MATCH:
    System.out.println(true);
  }
}
