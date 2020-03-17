function foo() {
    //ERROR: match
    x = <div a="foo" />;
    //ERROR: match
    x = <div a="foo" ></div>;
    //TODO: less is ok
    x = <div a="foo" >this has text</div>;
}
