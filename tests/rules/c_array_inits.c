int foo() {
    // ruleid: c-array-initialization
    char buf[64];
    // ruleid: c-array-initialization
    char buf[64] = "foo bar baz...";
    // ruleid: c-array-initialization
    char buf[64] = {0};
}
