import org.foo.Foo;

public class Test {
    public static void main(String[] args) {
        Foo foo = new Foo();
        // ruleid: metavariable-resolution-test
        foo.bar();
        foo.baz();
    }
}
