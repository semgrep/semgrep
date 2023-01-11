public class Foo {
  public static void main(String args[]) {
        // Empty parentheses and curly braces and no return
        Operations greet = () -> { System.out.println("Hello"); };
    }

    interface Operations {
        void operation();
    }
}
