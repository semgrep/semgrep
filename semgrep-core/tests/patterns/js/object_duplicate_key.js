// https://github.com/returntocorp/semgrep/issues/3579

//ERROR:
var foo = {
    bar: "baz",
    bar: "qux"
};

//ERROR:
var foo = {
    "bar": "baz",
    bar: "qux"
};

//ERROR:
var foo = {
    0x1: "baz",
    1: "qux"
};

//OK:
var foo = {
    bar: "baz",
    quxx: "qux"
};

