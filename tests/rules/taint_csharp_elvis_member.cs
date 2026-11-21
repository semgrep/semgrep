class Box { public string Value; }

class C {
    void Foo(Box? b) {
        b.Value = source();
        string s = b?.Value;
        // ruleid: taint
        sink(s);
    }

    void Safe(Box? b) {
        string s = b?.Value;
        // OK: taint
        sink(s);
    }
}
