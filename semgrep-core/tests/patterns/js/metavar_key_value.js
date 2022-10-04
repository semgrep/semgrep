function foo() {
    // ERROR:
    var foo = {key: value, key2: value2, key3: value3};

    // ERROR:
    var bar = {foo: bar, foo2: bar2, foo3: bar3};
}
