package Foo

type person struct {
    name string
    age  int
}

func bar() {
    var a[5] person
    var p person

    //ERROR:
    a = b;
    p = b;
}
