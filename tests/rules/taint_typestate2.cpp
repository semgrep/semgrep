void test1(Foo *p) {
    // ok: test
    delete p;
    // ruleid: test
    delete p;
}

void test2(Foo *p) {
    // ok: test
    delete p->field;
    // ruleid: test
    delete p->field;
}
