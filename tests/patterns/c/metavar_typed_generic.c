int test_equal(int a, int *p, char *x) {
    int b = 2;
    if (a == b) return 1;
    if (3 == 4) return 2;

    char c = 'a';
    char d = 'b';
    if (c == d) return 0;

    int *q = malloc(sizeof(int));
    //ERROR: match
    if (p == q) return 1;
    //ERROR: match
    if (p == NULL) return 1;

    if (p == b) return 2;

    char *y = "bye";
    //ERROR: match
    if (x == y) return 2;
    //ERROR: match
    if (x == "nope") return 3;
    //ERROR: match
    if ("lit1" == "lit2") return 4;

    //ERROR: match
    if (NULL == NULL) return 5;
    return 0;
}

