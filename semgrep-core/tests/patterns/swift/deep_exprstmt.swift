func foo() {
    //ERROR: match
    foo();
    bar();
    //ERROR: match
    foo();
    let x = bar();
    //ERROR: match
    foo()
    print(bar());
    //ERROR: match
    foo();
    return bar();
}
