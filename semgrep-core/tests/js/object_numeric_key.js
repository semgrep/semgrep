// https://github.com/returntocorp/semgrep/issues/3579

//ERROR:
var foo = {
    0x1: "foo"
};

//ERROR:
var bar = {
    1: "bar"
};

//ERROR:
var baz = {
    1.0: "baz"
};

//OK:
var xyz = {
    2: "xyz"
};
