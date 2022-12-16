public class Foo {
  public static void main(String args[]) {
        // with type declaration
        Operations addition = (int a, int b) -> a + b;
    }

    interface Operations {
        int operation(int a, int b);
    }
}
