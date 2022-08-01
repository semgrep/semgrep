func foo() {
    //ERROR:
    let user_data = get();
    print("do stuff");
    foobar();
    eval(user_data);
}
