var tainted = source();

function foo(x=tainted + "foo") {
    // ruleid: test-js
    sink(x);
}

foo();

function bar(x=source()) {
    // ruleid: test-js
    sink(x);
}

bar();

function baz(x="safe") {
    // ok: test-js
    sink(x);
}

baz();
