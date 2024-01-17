void f() {
    // ok: placement-new
    new int;
    // ok: placement-new
    new int[10];
    // ruleid: placement-new
    new (storage) int;
}
