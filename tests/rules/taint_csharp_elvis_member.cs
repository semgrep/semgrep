class Box { public string Value; public string Other; }

class C {
    void Foo(Box? b) {
        b.Value = source();
        string s = b?.Value;
        // ruleid: taint
        sink(s);
    }

    // Only `b.Other` is tainted: reading `b?.Value` stays clean.
    void Safe(Box? b) {
        b.Other = source();
        string s = b?.Value;
        // ok: taint
        sink(s);
    }
}
