public class Foo {
   public static void main() {
      //ERROR: match
      int x;
      f(x);

      //ERROR: match
      String y;
      float z;
      f(y);
   }
}
