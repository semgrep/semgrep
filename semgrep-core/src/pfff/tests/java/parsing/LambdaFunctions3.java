public class Foo {
  public static void main(String args[]) {
        // with return statement and curly braces
        Operations multiplication = (int a, int b) -> {return a * b; };
    }

    interface Operations {
        int operation(int a, int b);
    }
}
