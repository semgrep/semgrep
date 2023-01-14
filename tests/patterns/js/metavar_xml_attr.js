
function foo() {
    //ERROR: match
    x = <div a="foo" b="bar" />;

    x = <div a="foo" ></div>;

    x = <div b="bar" ></div>;

    //ERROR: match
    x = <div b="bar" a="foo" />;

}