package Foo 

func foo() {
    // ERROR:
    foo = config {
        key: value,
        key2: value2,
        key3: value3
    }

    bar = config2 {
        key: value,
    }
}
