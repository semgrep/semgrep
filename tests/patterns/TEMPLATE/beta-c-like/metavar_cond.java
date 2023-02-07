class Foo {
void foo() {
    x = 1;
    //ERROR:
    if (x > 2)
        foo();
}

}