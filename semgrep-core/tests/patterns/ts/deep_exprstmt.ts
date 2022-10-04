function foo() {
    //ERROR: match
    foo();
    bar();
    //ERROR: match
    foo();
    x = bar();
    //ERROR: match
    foo();
    print(bar());
    //ERROR: match
    foo();
    await bar();
    //ERROR: match
    foo();
    bar().then(other => stuff());
    //ERROR: match
    foo();
    return bar();
}
