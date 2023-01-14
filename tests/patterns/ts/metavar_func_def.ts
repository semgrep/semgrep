// ERROR:
function foo() {
    foo(1,2);
}

// ERROR:
function bar(bar1:number,bar2:number,bar2:number) {
    foo(1,2);
    bar(1,2,3);
}

// ERROR:
function foobar(bar1: number): number {
    foo();
}
