int main() {
    // nosem: rules.test-nosem
    test_nosem_func();
    test_nosem_func(); // nosem: rules.test-nosem
    test_nosem_func();
    return 0;
}
