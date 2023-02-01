local foo = function () [
    //ERROR:
    foo("whatever sequence of chars"),
    //ERROR:
    foo('whatever sequence of chars'),
    //ERROR:
    foo(|||whatever sequence of chars
        |||),
];
foo()
