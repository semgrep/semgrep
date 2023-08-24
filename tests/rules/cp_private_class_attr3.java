class Test1 {

    private int c;
    private int x;
    private int y;

    public Test1() {
        this.c = 42;
        this.y = 42;
    }

    public void doSomething1() {
        x = 42;
        y = 0;
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

class Test2 {

    private int x;

    public Test2() {
        this.x = 42;
    }

    public Test2(int v) {
        // nothing
    }

    public void doSomething4(){
        //ok:test
        foo(x);
    }

}
