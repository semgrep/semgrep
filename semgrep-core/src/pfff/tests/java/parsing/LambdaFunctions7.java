public class Foo {
  public static void main(String args[]) {
        // Empty parentheses and no curly braces
        Operations greet = () -> "Hello";
    }

    interface Operations {
        String operation();
    }
}
