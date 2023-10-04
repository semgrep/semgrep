public class boxing_equivalences_int {
  public static void main(String[] args) {
    int i1 = 4;
    Integer i2 = 5;
    java.lang.Integer i3 = 6;
    // MATCH:
    System.out.println(i1);
    // MATCH:
    System.out.println(i2);
    // MATCH:
    System.out.println(i3);
    // MATCH:
    System.out.println(7);
  }
}
