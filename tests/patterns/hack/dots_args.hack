function foo() {
    //ERROR:
    foo(1,2,3,4,5);
    //ERROR:
    foo(5);

    $fun = 5;
    //ERROR: 
    foo($test, $fun);

    foo($not_this);
}
