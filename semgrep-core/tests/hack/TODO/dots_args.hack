function foo() {
  //MATCH:
    foo(1,2,3,4,5);
  //MATCH:
    foo(5);
  //MATCH:  
    $fun = 5;
    foo($test, $fun);

    foo($not_this);
}
