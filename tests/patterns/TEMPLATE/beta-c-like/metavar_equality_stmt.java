class foo {

void foo() {
    //ERROR:
    if (x > 2) {
        return 1;
    } else {
        return 1;
    }

    if (x > 2) {
        return 1;
    } else {
        return 2;
    }
}


}
