public class Foo {
  public static void main(String args[]) {
        // Empty parentheses and curly braces and return
        Operations greet = () -> { return "Hello"; };
    }

    interface Operations {
        String operation();
    }
}
