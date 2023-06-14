class Test {

    private static int s;

    private int x;

    static {
        s = 42;
    }

    public void doSomething1() {
        x = 42;
    }

    public void doSomething2(){
        //ok:test
        foo(x);
    }

    public void doSomething3(){
        //ruleid:test
        foo(s);
    }

}
