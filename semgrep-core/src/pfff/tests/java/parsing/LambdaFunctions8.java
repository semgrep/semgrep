public class Foo {
  public static void main(String args[]) {
        // Empty parentheses and no curly braces
        Operations greet = () -> System.out.println("Hello");
    }

    interface Operations {
        void operation();
    }
}
