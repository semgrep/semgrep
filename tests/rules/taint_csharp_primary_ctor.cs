public class Container(string data, string other) { }

public class C {
    public void Foo() {
        var c = new Container("", "");
        c.data = source();
        // ruleid: taint
        sink(c.data);
    }

    // Field-sensitive: taint on another member does not reach `c.data`.
    public void Safe() {
        var c = new Container("", "");
        c.other = source();
        // ok: taint
        sink(c.data);
    }
}

public record Rec(string Value);

public class D {
    public void Foo() {
        var r = new Rec("");
        r.Value = source();
        // ruleid: taint
        sink(r.Value);
    }
}
