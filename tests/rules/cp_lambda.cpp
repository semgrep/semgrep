void test() {
    int v = 42;
    // ruleid: test
    run([v]() { foo(v); });

void test() {
    int v = 42;
    // v is modified in the lambda
    run([&v](int x) { v = x; });
    // ok: test
    foo(v);
}
