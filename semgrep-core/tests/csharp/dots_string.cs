public static void Bar(input) {
    //ERROR:
    Foo("whatever sequence of chars");

    //OK:
    Foo("not a constant string: " + input);
}

