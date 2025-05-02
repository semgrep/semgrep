import a.b.Foo;

class Test {
    void test() {
        // ruleid: metavar-name-imported-entity-java
        Foo.x();
    }
    void test2() {
        // ruleid: metavar-name-imported-entity-java
        Foo foo = new Foo();
    }
}
