int foo() {
    // ruleid: cpp-array-initialization
    char buf[64];
    // ruleid: cpp-array-initialization
    char buf[64] = "foo bar baz...";
    // ruleid: cpp-array-initialization
    char buf[64] = {0};
}
