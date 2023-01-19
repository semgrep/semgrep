void test_equal() {
    a = 1;
    b = 2;
    //ERROR: match
    if (a+b == a+b)
        return 1;
    return 0;
}

