public class Person {
    FooType default_age;

    void check(FooType age) {
        age = default_age;
        //ERROR:
        check(age);
        //ERROR:
        check(this.default_age);
    }
}

