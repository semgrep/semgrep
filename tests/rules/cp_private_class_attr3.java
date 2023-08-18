class Test {

    private int c;
    private int x;
    private int y;

    public Test() {
        this.c = 42;
        this.y = 0;
    }

    public void doSomething1() {
        x = 42;
        y = 1;
    }

    public void doSomething2(){
        //ok:test
        foo(x);
        //ok:test
        foo(y);
    }

    public void doSomething3(){
        //ruleid:test
        foo(c);
    }

}
