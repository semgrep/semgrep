function foo() {
    //ERROR: match
    x = <div a="foo" />;
    //ERROR: match
    x = <div a="foo" ></div>;
    //ERROR: less is ok
    x = <div a="foo" >this has text</div>;
}
