function foo() {
 //ERROR:
    foo(1,2);

 //ERROR:
 foo(1,
     2);

 //ERROR:
 foo (1, // comment
      2);

 foo(2,1);
}
