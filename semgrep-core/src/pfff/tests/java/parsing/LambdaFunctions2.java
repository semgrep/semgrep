public class Foo
{
  public static void main(String args[]) {
        // without type declaration
        Operations subtraction = (a, b) -> a - b;
    }

    interface Operations {
        int operation(int a, int b);
    }
}
