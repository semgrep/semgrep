public class Foo {
  public static void main(String args[]) {
        // Empty parentheses and no curly braces
        Operations greet = () -> {System.out.println("Hello"); System.out.println("World");};
    }

    interface Operations {
        void operation();
    }
}
