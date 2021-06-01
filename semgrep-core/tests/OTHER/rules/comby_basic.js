function foo() {
 //ERROR:
    foo(1,2);

 //TODO should detect that:
 foo(1,
     2);

 //TODO that one too:
 foo (1, // comment
      2);

 foo(2,1)
}
