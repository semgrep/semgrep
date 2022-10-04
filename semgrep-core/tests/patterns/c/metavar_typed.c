int test_equal() {
    int a = 1;
    int b = 2;
    if (a == b) return 1;
    if (3 == 4) return 2;

    int *p = malloc(sizeof(int));
    int *q = malloc(sizeof(int));
    if (p == q) return 1;

    char *x = "hello";
    char *y = "bye";
    //ERROR: match
    if (x == y) return 2;
    //ERROR: match
    if (x == "nope") return 3;
    //ERROR: match
    if ("lit1" == "lit2") return 4;
    return 0;
}

