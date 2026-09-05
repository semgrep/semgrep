public class Container(string data) { }

public class C {
    public void Foo() {
        var c = new Container("");
        c.data = source();
        // ruleid: taint
        sink(c.data);
    }

    public void Safe() {
        var c = new Container("");
        // OK: taint
        sink(c.data);
    }
}
