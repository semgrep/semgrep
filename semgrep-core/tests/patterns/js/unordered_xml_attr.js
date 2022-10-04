
function foo() {
    //ERROR: match
    x = <div a="foo" b="bar" />;
    //ERROR: order does not matter
    x = <div b="bar" a="foo" />;
}