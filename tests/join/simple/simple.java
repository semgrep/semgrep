class Foo {
    boolean is_safe;

    public Foo(boolean is_safe) {
        this.is_safe = is_safe;
    }

    public void exec() {
        System.out.println("exec");
    }
}

class A extends Foo {
    public A() {
        super(true);
    }
}

class B extends Foo {
    public B() {
        super(false);
    }
}

class Test {
    public static void main() {
        new A().exec();

        // MATCH:
        new B().exec();
    }
}