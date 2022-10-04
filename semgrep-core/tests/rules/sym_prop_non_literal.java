public class test {
    public static void main() {
        Object a = x(); // x, y, z are a static function in class Hello
        Object b = a.y();
        // ruleid: sym_prop_non_literal 
        Object d = b.z();
    }
}