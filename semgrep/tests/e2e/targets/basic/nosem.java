class NoSem {
    public static void main(String[] args) {
        // nosem: rules.test-nosem
        test_nosem_func();
        test_nosem_func(); // nosem: rules.test-nosem
        test_nosem_func();
    }
}
