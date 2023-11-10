// These aren't really boxing equivalences since Java has no primitive string
// type, but they essentially follow the same pattern as the other
// boxing_equivalences tests, and String deserves special handling because it is
// used pervasively and is a builtin in many other languages.
public class boxing_equivalences_string {
  public static void main(String[] args) {
    String s1 = "asdf";
    java.lang.String s2 = "asdf";
    // MATCH:
    System.out.println(s1);
    // MATCH:
    System.out.println(s2);
    // MATCH:
    System.out.println("asdf");
  }
}
