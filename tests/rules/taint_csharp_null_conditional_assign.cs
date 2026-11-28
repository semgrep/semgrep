class Box { public string Value; public string Other; public Box Inner; public string[] Items; }

class C {
    // C# 14 null-conditional assignment writes through to `b`.
    void Write(Box? b) {
        b?.Value = source();
        // ruleid: taint
        sink(b.Value);
    }

    void Nested(Box? b) {
        b?.Inner?.Value = source();
        // ruleid: taint
        sink(b.Inner.Value);
    }

    void Indexed(Box? b) {
        b?.Items[0] = source();
        // ruleid: taint
        sink(b.Items[0]);
    }

    // The write only happens when `b` is not null, so a clean value
    // does not remove earlier taint (weak update).
    void WeakUpdate(Box b) {
        b.Value = source();
        b?.Value = clean();
        // ruleid: taint
        sink(b.Value);
    }

    void Clean(Box? b) {
        b?.Value = clean();
        // ok: taint
        sink(b.Value);
    }

    // Compound null-conditional assignments write through `b` too.
    void Compound(Box? b) {
        b?.Value += source();
        // ruleid: taint
        sink(b.Value);
    }

    void NullCoalescing(Box? b) {
        b?.Value ??= source();
        // ruleid: taint
        sink(b.Value);
    }

    void ElementAccess(string[]? arr) {
        arr?[0] = source();
        // ruleid: taint
        sink(arr[0]);
    }

    // A call as the guard is evaluated once, into a temporary.
    void CallGuard() {
        Make()?.Value = source();
        // ok: taint
        sink(Make().Value);
    }

    // Field-sensitive: another field of `b` stays clean.
    void OtherField(Box? b) {
        b?.Value = source();
        // ok: taint
        sink(b.Other);
    }

    // The right-hand side is still evaluated (under the guard).
    void SinkInRhs(Box? b) {
        // ruleid: taint
        b?.Value = sink(source());
    }
}
