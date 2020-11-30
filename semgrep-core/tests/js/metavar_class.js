// ERROR:
class Foo {
    constructor(foo) {
        this.foo = foo;
    }

    get foo() {
        return this.foo;
    }
}

// ERROR:
class Bar extends Foo {
    constructor (bar) {
        this.bar = bar;
    }
}
