public class Foo {
  public static void main(String args[]) {
        // without return statement and curly braces
        Operations division = (int a, int b) -> { System.out.println(a /b); };
    }

    interface Operations {
        void operation(int a, int b);
    }
}
