void foo() {
    [a] { return a; };
    int x = { [x] = 2 };
    int y = { [a] { return a; } };

}
