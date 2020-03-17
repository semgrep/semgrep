
function foo() {
    //ERROR: match
    x = <div dangerouslySetInnerHTML="foo" />;
    //ERROR: match
    x = <div dangerouslySetInnerHTML="foo" ></div>;

    //ERROR: less is ok
    x = <div dangerouslySetInnerHTML="foo" >this has text</div>;
    //ERROR: order does not matter
    x = <div dangerouslySetInnerHTML="foo" b="2" />;
    //ERROR: order does not matter
    x = <div a="1" dangerouslySetInnerHTML="foo" b="2" />;
}