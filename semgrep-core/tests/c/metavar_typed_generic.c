int test_equal(int a, int *p, char *x) {
    int b = 2;
    if (a == b) return 1;
    if (3 == 4) return 2;

    int *q = malloc(sizeof(int));
    //ERROR: match
    if (p == q) return 1;
    //ERROR: match
    if (p == NULL) return 1;

    char *y = "bye";
    //ERROR: match
    if (x == y) return 2;
    //TODO: match
    if (x == "nope") return 3;
    //TODO: match
    if ("lit1" == "lit2") return 4;
    return 0;
}

