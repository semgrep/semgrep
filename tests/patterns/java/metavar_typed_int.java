public class Main {
  public static void main(String[] args) {
    // MATCH:
    System.out.println(1);
    // MATCH:
    System.out.println(1 + 1);
    // MATCH:
    System.out.println(1 - 1);
    // TODO:
    System.out.println(1 * 1);
    // TODO:
    System.out.println(1 / 1);
    // TODO:
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
  }
}
