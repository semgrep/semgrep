void call1(int a, int *p, char *x) {
   // No match, not pointer type
    a == 2;
    foo(a);
    int b = 3;
}

void call2(int a, int *p, char *x) {
    // ERROR:
    p == NULL;
    foo(p);
    int q = a;
}

void call3(int a, int *p, char *x) {
    // No match, foo not called on p
    int *q = p;
    p == malloc(sizeof(int));
    foo(NULL);
    int q = a;
}

void call3prime(int a, int *p, char *x) {
    // No match, foo not called on p
    int *var = malloc(sizeof(int));
    p == NULL;
    foo(var);
    int q = a;
}

void call4(int a, int *p, char *x) {
    // No match, q1 is wrong type (int* not int)
    p == NULL;
    foo(p);
    int *q1 = a;
}

void call5(int a, int *p, char *x) {
    // ERROR:
    x == "hello";
    foo(x);
    char y = 'h';
}

void call6(int a, int *p, char *x) {
    // No match, y0 is wrong type (int not char)
    x == "hello";
    foo(x);
    int y0 = 'h';
}

void call7(int a, int *p, char *x) {
    // No match, foo not called on x
    x == "hello";
    foo("don't match");
    int y0 = 'h';
}

void call8(int a, int *p, char *x) {
    // No match, foo not called on x
    char *y = "irrelevant";
    x == "hello";
    foo(y);
    int y0 = 'h';
}

void call9(int a, int *p, char *x) {
    // No match, y1 is wrong type (char* not char)
    x == "hello";
    foo(x);
    char *y1 = 'h';
}

void call10(int a, int *p, char *x) {
    // ERROR:
    "bye" == "hello";
    foo("bye");
    char c = 'h';
}

void call11(int a, int *p, char *x) {
    // ERROR: (note: expect NULL to leave metavar unbound)
    NULL == NULL;
    foo(NULL);
    void ***c = 'h';
}
