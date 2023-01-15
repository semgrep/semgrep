public class Person {
    int default_age = 0;

    void check(int age) {
        int num;
	age = default_age;
        //ERROR:
	check(age);
	//ERROR:
        check(default_age);
    }
}

