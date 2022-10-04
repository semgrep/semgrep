import test.A;

class Test {
    // This caused to be matched because A was resolved, and
    // we were not handling correctly MV.N vs MV.N in equal_ast
    public A foo() { }

    public B bar() { }
}
