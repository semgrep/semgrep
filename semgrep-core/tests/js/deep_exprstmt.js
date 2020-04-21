function foo() {
    //ERROR: match
    bar();
    //ERROR: match
    x = bar();
    //ERROR: match
    print(bar());
}
