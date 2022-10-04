// ERROR:
function foo() {
    foo(1,2);
}

// ERROR:
function bar(bar1, bar2, bar3) {
    foo(1,2);
    bar(1,2,3);
}

