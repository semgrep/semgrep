public class Foo {
    public static void main() {
        String x;
        String y;
        int a;
        int b;
        //ERROR: match
        if (x == y) x = y;
        if (a == b) a = b;
   }
}
