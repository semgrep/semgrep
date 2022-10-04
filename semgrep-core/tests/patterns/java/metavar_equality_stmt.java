class foo {

void foo() {
    //ERROR:
    if (x > 2) {
        foo();
        bar();
    } else {
        foo();
        bar();
    }

    if (x > 2) {
        foo();
        bar();
    } else {
        foo();
    }
}


}